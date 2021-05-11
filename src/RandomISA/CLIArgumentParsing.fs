namespace RandomISA

open Argu

module CLIArgumentParsing =
  
    type CLIArguments =
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-i")>]   FileList            of path:string
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-min")>] MinimalAssayLentgh  of number:int
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-max")>] MaxmialAssayLentgh  of number:int
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-o")>]   OutputFile          of path:string
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-t")>]   TechnicalReplicates of number:int

    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | FileList _            -> "specify file with the input file list"
                | MinimalAssayLentgh _  -> "specify the minimal assay file length"
                | MaxmialAssayLentgh _  -> "specify the maximal assay file length"
                | OutputFile _          -> "specify output directory"
                | TechnicalReplicates _ -> "specify the number of assay files generated"