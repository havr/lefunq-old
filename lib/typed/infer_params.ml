open Base
open Node

let destruct ~ctx sh = 
    let rec destruct' = function
    | Node.Destruct.Name n ->
        let typ = Infer_ctx.(ctx.tempvar()) in
        let id = Scope.add_local_binding ctx.scope n.given in
        let node' = Node.Destruct.Name Node.Destruct.{n with scope = id; typ} in
        Infer_ctx.add_env ~ctx id (Type.make_scheme [] typ);
        (node', typ)
    | Node.Destruct.Unit ->
        (Node.Destruct.Unit, Type.Unit)
    | Node.Destruct.Tuple tup -> 
        let nodes, typs = List.map tup ~f: destruct'
        |> List.unzip in
        (Node.Destruct.Tuple nodes), Type.Tuple (typs)
    in destruct' sh

let resolve ~ctx ~expr param = 
    (* TODO: provide proper typename range *)
    match Param.(param.value) with
    | Param.Positional p -> 
        let shape, typ = destruct ~ctx p.shape in
        Param.{typ; value = Param.Positional {shape}}
    | Param.Optional p -> 
        (match p.default with
        | Some e ->
            let node, typ = expr e in
            let scope = Scope.add_local_binding ctx.scope p.given.value in
            Infer_ctx.add_env ~ctx scope (Type.make_scheme [] typ);
            Param.{
                (* Some typ? *)
                typ;
                value = Param.Optional {
                    p with
                    default = Some node;
                    scope;
                }
            }
        | None ->
            let typ = ctx.tempvar() in
            let scope = Scope.add_local_binding ctx.scope p.given.value in
            Infer_ctx.add_env ~ctx scope (Type.make_scheme [] typ);
            Param.{
                typ;
                value = Param.Optional { p with scope }
            }
        )
    | Param.Named n -> 
        let shape, typ = destruct ~ctx n.shape in
        Param.{typ; value = Param.Named {n with shape}}
    | Param.Extension _ -> (raise Common.TODO)


let to_type params = 
    List.map params ~f: Param.(fun {typ; value; _} ->
        match value with
            | Positional _ -> Type.PosParam typ
            | Named {given; _} -> Type.NamedParam {param_typ = typ; is_optional = false; param_name = given.value}
            | Optional {given; _} -> Type.NamedParam {param_typ = typ; is_optional = true; param_name = given.value}
            | Extension _ -> (raise Common.TODO) (* flatmap struct here *)
)

let to_lambda params result = 
    Type.Lambda.make (to_type params) result