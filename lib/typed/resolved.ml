open Common
open Base
module Binding = struct
    type t = 
        | Local of string 
        | Global of {id: Id.t; scheme: Type.scheme}

    let make id scheme = Global {id; scheme}
end

module Typedef = struct 
    type param = {var: string}

    type t = {
        id: Id.t;
        params: param list;
        def: Type.def
    }

    let make id params def = {id; params; def}
end

module Module = struct
    type exposed = {
        binding: Binding.t option;
        modu: t option;
        typedef: Typedef.t option
    }

    and t = {
        id: Id.t;
        exposed: exposed StringMap.t;
    }

    let empty_exposed = {binding = None; modu = None; typedef = None}

    let make id exposed = {id; exposed }

    let empty id = make id (Map.empty(module String))

    let rec lookup exposed = function
    | [] -> raise (Invalid_argument "no path provided")
    | name :: [] ->
        Map.find exposed name
    | modu :: rest -> (
        match Map.find exposed modu with
        | None -> None
        | Some exposed -> (
            match exposed.modu with 
            | None -> None
            | Some m -> lookup m.exposed rest)
    )
end
