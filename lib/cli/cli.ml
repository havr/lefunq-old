module Args = struct
    module State = struct 
        type t = {list: string list}
        let make arg_list = {list = arg_list}
    end

    type 't t = {fn: State.t -> ('t * State.t, string) result}

    let eval arg_list p = State.make arg_list |> p.fn

    let map p f = {fn = fun state ->
        match p.fn state with
        | Error e -> Error e
        | Ok(pv, state') -> Ok(f pv, state')
    }

    let product a b = {fn = fun state ->
        match a.fn state with
        | Error e -> Error e
        | Ok(av, state') ->
            match b.fn state' with
            | Error e -> Error e
            | Ok(bv, state'') -> Ok ((av, bv), state'')
    }

    let pop = { fn = fun state ->
        match state.list with
        | [] -> Error "!!!TODO: expected argument not found"
        | a :: rest -> Ok (a, {list = rest})
    }

    module Syntax = struct 
        let (let+) = map
        let (and+) = product
    end
end

type 'a subcommand = {
    name: string;
    aliases: string list;
    description: string;
    cmd: string list -> unit
}

module type Command = sig 
    type t

    val args: t Args.t
    val main: t -> unit
end

let subcommands config =
    match Array.to_list @@ Sys.argv with
    | [] -> print_endline "unreachable"
    | [_] -> print_endline @@ String.concat " | " (List.map (fun subc -> subc.name) config)
    | _ :: name :: rest ->
        match List.find_opt (fun subc -> subc.name = name) config with
        | None -> print_endline @@ "unknown command: " ^ name |> ignore
        | Some subc -> subc.cmd rest

let run (module Cmd: Command) argv =
    match Args.eval argv Cmd.args with
    | Error e -> print_endline e
    | Ok (config, _) -> Cmd.main config
