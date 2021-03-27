open Base
open Common

module Lexer = struct 
    module State = struct
        open Base

        type t = {stream: string; pos: Pos.t}

        let isEof state = state.pos.idx >= String.length state.stream
        let isNotEof state = not (isEof state)

        let make str = {stream = str; pos = Pos.{row = 1; col = 1; idx = 0}}

        let curr lexer = String.get lexer.stream lexer.pos.idx
        let next state = {state with pos = Pos.next (curr state) state.pos}

        let cut_lex start end_ = 
            let value = String.sub start.stream ~pos:(start.pos.idx) ~len:(end_.pos.idx - start.pos.idx) in value

        (* TODO: curr returns EOF if eof (no isEof fn) *)
        let describe_current state = if isEof state then 
                "EOF" 
            else let ch = curr state in match ch with
                | '\n' -> "newline"
                | '\t' -> "tab"
                | _ -> Char.to_string ch
    end


    module Err = struct 
        type t = {pos: Pos.t; msg: string}
    end

    module Matcher = struct
        open Base

        let char set lexer = 
            if (State.isNotEof lexer) && (String.contains set (State.curr lexer))
                then (true, State.next lexer) 
                else (false, lexer)

        let str str state = 
            let len = String.length str in
            let rec loop state idx =  
                let ch = String.get str idx in
                if (State.isNotEof state) && (Char.equal ch (State.curr state))
                    then if (idx = len - 1) then (true, State.next state) else loop (State.next state) (idx + 1)
                    else (false, state)
            in loop state 0


        let maybe lexer state = let (_, state') = lexer state in (true, state')

        let many lexer state = 
            let rec loop (state: State.t) : (bool * State.t) = 
                let (ok, result) = lexer state in
                if ok then (loop result) else (true, state) 
            in loop state 

        let seq lexers state = List.fold ~init:(true, state) ~f:(fun (ok, state) lexer -> 
            if ok then lexer state else (ok, state)
        ) lexers

        let oneMore lexer state = let (ok, next) = lexer state in
            if not ok then 
                (false, state)
            else 
                (* let rec loop (state: t) : (bool * t) = 
                    let (ok, result) = lexer state in
                    if ok then (loop result) else (true, state)
                in
                loop next  *)
                many lexer next

        let not matcher state = 
            match matcher state with
            | (true, _) -> (false, state)
            | (false, _) -> (true, (State.next state))

        let choice matchers initial = 
            let found = List.find_map matchers ~f: (fun matcher -> 
                let (ok, state) = matcher initial in
                if ok then Some state else None
            ) in match found with
                | Some state -> (true, state)
                | None -> (false, initial)

        let ignore defs state =
            List.find_map defs ~f:(fun matcher -> 
                let (ok, state') = matcher state in
                if ok then Some(state') else None
            )

        let next defs state = 
            let matched = List.find_map defs ~f:(fun (make, matcher) -> 
                let (ok, state') = matcher state in
                if ok then Some (make, state') else None
            ) in match matched with
            | None -> Error Err.{pos = State.(state.pos); msg = "unexpected character " ^ (State.describe_current state)}
            | Some (make, state') ->
                let contents = String.sub state.stream ~pos: state.pos.idx ~len: State.(state'.pos.idx - state.pos.idx) in
                Ok (state', Span.make state.pos state'.pos (make contents))
    end

    module Match = struct
    type 't config = {
        lexemes: ((string -> 't) * (State.t -> (bool * State.t))) list;
        skip: (State.t -> (bool * State.t)) list;
        unexpected: string -> 't;
        eof: 't 
    }

    exception Panic of string

    let lexeme_of map state state' =
        if not @@ phys_equal State.(state.stream) State.(state'.stream) then
            raise (Panic "states don't match")
        else Span.{
            range = {
                start = state.pos;
                end' = State.(state'.pos)
            };
            value = map(String.sub State.(state.stream) ~pos: state.pos.idx ~len: State.(state'.pos.idx - state.pos.idx))
        }        

    let next config state = 
        let skip state = List.find_map config.skip ~f:(fun matcher -> 
            let (ok, state') = matcher state in
            if ok then Some(state') else None
        ) in
        let match_ state = 
            List.fold ~init:None config.lexemes ~f:(fun result (make, matcher) -> 
                match matcher state with
                | (false, _) -> result
                | (true, state') ->
                    let length = state'.pos.idx - state.pos.idx in
                    match result with
                    | None -> Some(length, make, state')
                    | Some(largest, _, _) -> 
                        if largest < length then 
                            Some(length, make, state')
                        else result
            )
        in
        let rec match_step (state, prev_mismatch, result) =
            match skip state with
            | Some state' -> 
                let new_result = match prev_mismatch with
                | Some mismatch ->(lexeme_of (fun s -> config.unexpected s) mismatch state) :: result
                | None -> result
                in match_step (state', None, new_result)
            | None ->
                if State.isEof state then 
                    let eof_lexeme = lexeme_of (fun _ -> config.eof) state state in
                    let new_result = match prev_mismatch with
                        | Some mismatch -> eof_lexeme :: lexeme_of (fun s -> config.unexpected s) mismatch state :: result
                        | None -> eof_lexeme :: result
                    in (List.rev new_result, state)
                else match match_ state with
                | Some (_, map, state') -> 
                    let lexeme = lexeme_of map state state' in
                    let lexemes = List.rev @@ match prev_mismatch with 
                        | Some mismatch -> lexeme :: lexeme_of (fun s -> config.unexpected s) mismatch state :: result
                        | None -> lexeme :: result
                    in (lexemes, state')
                | None ->
                    let new_mismatch = if Option.is_none prev_mismatch then Some state else prev_mismatch in
                    match_step (State.next state, new_mismatch, result)
        in
        let (lexemes, state') = match_step (state, None, []) in (lexemes, state')

    let all config state = 
        let nexter = next config in
        let rec loop (result, state) = 
            match State.isEof state with
            | true -> 
                Ok (List.rev result)
            | false ->
                let (lexemes, state') = nexter state in loop (lexemes @ result, state')
        in let result = loop ([], state) in
        result
    end

end

module type LEXEME = sig 
    type t

    val eof: t
    val to_string: t -> string
end

module Parser(Lexeme: LEXEME) = struct 
    type err = {
        err_msg: string;
        err_pos: Pos.t;
        caused_by: Lexeme.t;
        no_match: bool;
    }

    let err_to_string e = String.concat ~sep: " " [Pos.to_string e.err_pos; e.err_msg; "caused by"; Lexeme.to_string e.caused_by];

    module State = struct
        type t = { lexemes: Lexeme.t Span.t Array.t; curr: int; last_pos: Pos.t }

        let make lexemes = 
            let arr = Array.of_list lexemes in
            let length = Array.length arr in
            let value = if length = 0 then Pos.empty else Span.(arr.(length - 1).range.end') in 
            { curr = 0; lexemes = arr; last_pos = value }

        let curr state = 
            if state.curr < Array.length state.lexemes then
                state.lexemes.(state.curr)
            else 
                Span.make state.last_pos state.last_pos Lexeme.eof

        let next state = 
            if state.curr < Array.length state.lexemes then
                {state with curr = state.curr + 1}
            else 
                state

        let curr_pos state = (curr state).range.start
    end

    type 't t = { fn: State.t -> ('t * State.t, err) Result.t }

    let no_match err = { fn = fun state -> 
        Error {
            err_msg = err;
            err_pos = State.curr_pos state;
            caused_by = (State.curr state).value;
            no_match = true;
        }
    }
    
    let syntax_err err = { fn = fun state -> 
        Error {
            err_msg = err;
            err_pos = State.curr_pos state;
            no_match = false;
            caused_by = (State.curr state).value
        }
    }

    let bind parser f = { fn = fun state ->
        let result = parser.fn state in
            match result with
            | Error error -> Error error
            | Ok (result, state') -> (f result).fn state'
    }

    let eof = { fn = fun state ->
        if phys_equal (State.curr state).value Lexeme.eof then 
            Ok((), state)
        else (no_match "unable to match eof").fn state
    }

    let return value = { fn = fun state -> Ok (value, state) }

    let maybe parser = { fn = fun init_st ->
        match parser.fn init_st with
        | Ok (result, next_state) -> Ok(Some result, next_state)
        | Error error -> if error.no_match then Ok (None, init_st) else Error error
    }

    let many parser = { fn = fun state ->
        let rec loop result state = 
            match parser.fn state with
            | Ok (r, state') -> loop (r :: result) state'
            | Error e -> if e.no_match then Ok (List.rev result, state) else Error e
        in
            loop [] state
    }

    let some parser = { fn = fun state ->
        match parser.fn state with
        | Ok (result, state') -> begin
            match (many parser).fn state' with
            | Ok (results, state'') -> Ok (List.rev (result :: results), state'')
            | Error e -> Error e
            end
        | Error e -> Error e
    }

    let must matcher = { fn = fun state ->
        match matcher.fn state with 
        | Ok v -> Ok (v)
        | Error e -> Error (if e.no_match then {e with no_match = true} else e)
    }

    let one matcher = { fn = fun state ->
        match matcher (State.curr state) with 
        | Some mapped_value -> Ok(mapped_value, State.next state)
        | None -> (no_match "unexpected lexeme").fn state
    }

    let one_value matcher = { fn = fun state ->
        let curr_lex = (State.curr state) in
        match matcher curr_lex.value with 
        | Some mapped_value -> Ok({curr_lex with value = mapped_value}, State.next state)
        | None -> (no_match @@ "unexpected lexeme: " ^ (Lexeme.to_string curr_lex.value)).fn state
    }

    let product p r = {fn = fun state -> 
        match p.fn state with
        | Ok(p_result, state') -> begin
            match r.fn state' with
            | Ok(r_result, state'') -> Ok ((p_result, r_result), state'')
            | Error e -> Error e
        end
        | Error e -> Error e
    }

    let map p f = { fn = fun state ->
        match p.fn state with
        | Ok (result, state') -> Ok(f result, state')
        | Error e -> Error e
    }

    let wrap_fn fn = {fn = fun state -> (fn ()).fn state}
    module Infix = struct 
        let (>>=) = bind
    end

    module Syntax = struct
        let (let+) = map
        let (and+) = product
        let (let*) = bind
    end

    let one_more parser =
        let open Syntax in
        let+ first = parser
        and+ rest = many parser in
            first :: rest
end