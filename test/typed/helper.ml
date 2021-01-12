open Typed
open Base

let let_expr name stmt = Let.{
    given_name = name;
    scope_name = name;
    scheme = None;
    is_rec = false;
    block = Typed.Block.{
        stmts = [ stmt ]
    }
}

let let_stmt ?scope_name given_name stmts = Stmt.Let (
    Typed.Let.{
        given_name = given_name;
        scope_name = Option.value ~default:given_name scope_name;
        scheme = None;
        is_rec = false;
        block = Typed.Block.{ stmts }
    }
)

let value_stmt value typ = Stmt.Expr (
    Expr.Value (Value.{value; type_=typ})
)

let local_ident_stmt ?scope_name ?scheme name = 
    let ident = Ident.{
        pos = Common.Pos.empty;
        given_name = name;
        resolved = Ident.Local {
            param = false;
            scope_name = Option.value ~default:name scope_name
        };
        scheme = scheme
    } in Stmt.Expr (Expr.Ident ident)

let lambda_stmt params stmts = 
    let lambda = Typed.Lambda.{
        params = List.map params ~f:(fun (name, typ) -> 
            Typed.Param.{
              shape = Typed.Param.Name name;
              type' = typ
            }
        );
        block = Typed.Block.{stmts}
      }
    in Typed.Stmt.Expr (Typed.Expr.Lambda lambda)