open Cli

let () = subcommands [
    {
        name = "build";
        description = "builds the given module";
        aliases = ["b"];
        cmd = run (module Build)
    };
    {
        name = "run";
        description = "runs the given module";
        aliases = ["r"];
        cmd = run (module Run); 
    };
]