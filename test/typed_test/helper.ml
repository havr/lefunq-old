open Typed
open Base

let let_expr name stmt = Let.{
    range = Common.Span.empty_range;
    given_name = name;
    scope_name = name;
    scheme = None;
    is_rec = false;
    block = Typed.Block.{
        range = Common.Span.empty_range;
        stmts = [ stmt ]
    };
    params = [];
    result = Type.Unknown;
    sigt = None;
}

let let_stmt ?scope_name given_name stmts = Stmt.Let (
    Typed.Let.{
        range = Common.Span.empty_range;
        given_name = given_name;
        scope_name = Option.value ~default:given_name scope_name;
        scheme = None;
        is_rec = false;
        block = Typed.Block.{ stmts; range = Common.Span.empty_range };
        params = [];
        result = Type.Unknown;
        sigt = None;
    }
)

let value_stmt value typ = Stmt.Expr (
    Expr.Value (Value.{value; typ; range = Common.Span.empty_range})
)

let local_ident_stmt ?scope_name ?typ name = 
    let ident = Ident.{
        typ = Option.value ~default: Type.Unknown typ;
        range = Common.Span.empty_range;
        resolved = Symbol.Resolved.make name @@ Some (Typed.Symbol.Id.make "" [] @@ Option.value ~default:name scope_name);
        resolution = [];
    } in Stmt.Expr (Expr.Ident ident)

let lambda_stmt params stmts = 
    let lambda = Typed.Lambda.{
        range = Common.Span.empty_range;
        typ = Type.Unknown;
        params = List.map params ~f:(fun (n, typ) ->
            Typed.Param.{
              value = Typed.Param.(
                Positional {pattern = Name {given = n; scope = n}}
              );
              typ
            }
        );
        block = Typed.Block.{stmts; range = Common.Span.empty_range}
      }
    in Typed.Stmt.Expr (Typed.Expr.Lambda lambda)



(* TODO: move to Util.Lists ? *)
let difference ~equals src check = 
  List.fold src ~init: ([]) ~f: (fun a x ->
    match List.find check ~f: (equals x) with
    | Some _ -> a
    | None -> x :: a
  )

let simple name = Typed.Type.Simple (name, [])