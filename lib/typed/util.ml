module Basket = struct 
    type 'u t = {
        mutable items: 'u list
    }

    let make () = {items = []}

    let put basket item = 
        basket.items <- item :: basket.items

    let get basket = basket.items
end

type tempvar_gen = unit -> Type.t

let make_tempvar_gen prefix = 
    let idx = ref 0 in fun () ->
        let result = Type.Var (prefix ^ (Int.to_string !idx)) in
        idx := !idx + 1;
        result
