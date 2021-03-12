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
    Expr.Value (Value.{value; type_=typ; range = Common.Span.empty_range})
)

let local_ident_stmt ?scope_name ?scheme name = 
    let ident = Ident.{
        range = Common.Span.empty_range;
        given_name = name;
        resolved = Some (Typed.Symbol.Id.make "" @@ Option.value ~default:name scope_name);
        scheme = scheme
    } in Stmt.Expr (Expr.Ident ident)

let lambda_stmt params stmts = 
    let lambda = Typed.Lambda.{
        range = Common.Span.empty_range;
        params = List.map params ~f:(fun (name, typ) -> 
            Typed.Param.{
              shape = Typed.Param.Name name;
              type' = typ
            }
        );
        block = Typed.Block.{stmts; range = Common.Span.empty_range}
      }
    in Typed.Stmt.Expr (Typed.Expr.Lambda lambda)