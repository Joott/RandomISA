namespace RandomISA

open System
open System.IO
open CLIArgumentParsing
open Argu
open System.Reflection
open Deedle

module console1 =
    

    [<EntryPoint>]
    let main argv =
        let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)
        let parser = ArgumentParser.Create<CLIArguments>(programName =  (System.Reflection.Assembly.GetExecutingAssembly().GetName().Name),errorHandler=errorHandler)
        let directory = Environment.CurrentDirectory
        let getRelativePath (reference: string) (path: string) =
            Path.Combine(reference, path)
        let getPathRelativeToDir = getRelativePath directory
        let results = parser.Parse argv
        let i = results.GetResult FileList   |> getPathRelativeToDir
        let o = results.GetResult OutputFile |> getPathRelativeToDir
        let min = results.GetResult MinimalAssayLentgh
        let max = results.GetResult MaxmialAssayLentgh
        let t = results.GetResult TechnicalReplicates
        if File.Exists i then
            try
                printfn "Reading filenames"
                let fileNames: string [] =
                    Frame.ReadCsv (path = i, hasHeaders = true, separators = "\t")
                    |> Frame.getCol "ms_run"
                    |> Series.values
                    |> Array.ofSeq
                    |> Array.distinct
                printfn "Starting random proteomics assay generation"
                RandomISA.createRandomAssay fileNames min max t o
            with
            | ex -> printfn "%A" ex
        else
            failwith "The given path to the file list is no valid file path"
        printfn "Done"
        0

