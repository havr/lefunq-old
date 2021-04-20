let tempvar () = 
    let n = ref 0 in
    (fun () ->
        let s = "_$" ^ (Int.to_string @@ !n + 1) in
        n := !n + 1;
        s
    )