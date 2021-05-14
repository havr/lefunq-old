open Typed
open Base

let let_expr name stmt = Node.Let.{
    range = Common.Span.empty_range;
    given_name = name;
    scope_name = name;
    scheme = None;
    is_rec = false;
    block = Node.Block.{
        range = Common.Span.empty_range;
        stmts = [ stmt ]
    };
    params = [];
    result = Type.Unknown;
    sigt = None;
}

let let_stmt ?scope_name given_name stmts = Node.Stmt.Let (
    Node.Let.{
        range = Common.Span.empty_range;
        given_name = given_name;
        scope_name = Option.value ~default:given_name scope_name;
        scheme = None;
        is_rec = false;
        block = Node.Block.{ stmts; range = Common.Span.empty_range };
        params = [];
        result = Type.Unknown;
        sigt = None;
    }
)

let value_stmt value typ = Node.Stmt.Expr (
    Node.Expr.Value (Node.Value.{value; typ; range = Common.Span.empty_range})
)

let local_ident_stmt ?scope_name ?typ name = 
    let ident = Node.Ident.{
        typ = Option.value ~default: Type.Unknown typ;
        scheme = None;
        range = Common.Span.empty_range;
        qual = Typed_common.Qualified.just_name name @@ Some (Typed.Id.make "" [] @@ Option.value ~default:name scope_name);
    } in Node.Stmt.Expr (Node.Expr.Ident ident)

let lambda_stmt params stmts = 
    let lambda = Node.Lambda.{
        range = Common.Span.empty_range;
        typ = Type.Unknown;
        params = List.map params ~f:(fun (n, typ) ->
            Node.Param.{
              value = Node.Param.(
                Positional {shape = Name {given = n; scope = n; typ = typ}}
              );
              typ
            }
        );
        block = Node.Block.{stmts; range = Common.Span.empty_range}
      }
    in Node.Stmt.Expr (Node.Expr.Lambda lambda)

(* TODO: move to Util.Lists ? *)
let difference ~equals src check = 
  List.fold src ~init: ([]) ~f: (fun a x ->
    match List.find check ~f: (equals x) with
    | Some _ -> a
    | None -> x :: a
  )

let simple name = Type.Simple (Typed.Type.make_name (Typed.Typed_common.Qualified.just_name name None) Type.Foreign, [])
