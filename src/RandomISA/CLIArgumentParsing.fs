namespace RandomISA

open Argu

module CLIArgumentParsing =
  
    type CLIArguments =
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-i")>]   FileList           of path:string
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-min")>] MinimalAssayLentgh of number:int
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-max")>] MaxmialAssayLentgh of number:int
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-o")>]   OutputDirectory    of path:string
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-n")>]   NumberIterations   of number:int
        | [<Unique>] [<AltCommandLine("-c")>]                 Parallelism_Level  of level:int

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | FileList _           -> "specify file with the input file list"
                | MinimalAssayLentgh _ -> "specify the minimal assay file length"
                | MaxmialAssayLentgh _ -> "specify the maximal assay file length"
                | OutputDirectory _    -> "specify output directory"
                | NumberIterations _   -> "specify the number of assay files generated"
                | Parallelism_Level _  -> "set the number of cores the programm can use"