open Cli

let () = subcommands [
    {
        name = "build";
        description = "builds the given module";
        aliases = [];
        cmd = run (module Build)
    };
    {
        name = "run";
        description = "runs the given module";
        aliases = [];
        cmd = run (module Run); 
    };
]