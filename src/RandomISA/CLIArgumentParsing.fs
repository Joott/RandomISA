namespace RandomISA

open Argu

module CLIArgumentParsing =
  
    type CLIArguments =
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-i")>]   FileList                  of path:string
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-n")>]   NumberDifferentConditions of number:int
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-o")>]   OutputFile                of path:string
        | [<Mandatory>] [<Unique>] [<AltCommandLine("-t")>]   TechnicalReplicates       of number:int
        | [<Unique>] [<AltCommandLine("-m")>]                 TermMap                   of path: string
        | [<Unique>] [<AltCommandLine("-g")>]                 GenerateTermMap           of path: string
        | [<Unique>] [<AltCommandLine("-f")>]                 Investigation
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | FileList _                   -> "specify file with the input file list"
                | NumberDifferentConditions _  -> "specify the number of different conditions in the assay"
                | OutputFile _                 -> "specify output directory"
                | TechnicalReplicates _        -> "specify the number of assay files generated"
                | TermMap _                    -> "specify the path of the term map"
                | GenerateTermMap _            -> "specify the output path of the term map"
                | Investigation                -> "return process sequence in ISA-JSON Investigation file"