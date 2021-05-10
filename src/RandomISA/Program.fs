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
        let i = results.GetResult FileList        |> getPathRelativeToDir
        let o = results.GetResult OutputDirectory |> getPathRelativeToDir
        let min = results.GetResult MinimalAssayLentgh
        let max = results.GetResult MaxmialAssayLentgh
        let n = results.GetResult NumberIterations
        let c =
            match results.TryGetResult Parallelism_Level with
            | Some c    -> c
            | None      -> 1
        Directory.CreateDirectory(o) |> ignore
        if File.Exists i then
            try
                printfn "Reading filenames"
                let fileNames: string [] =
                    Frame.ReadCsv (path = i, hasHeaders = true, separators = "\t")
                    |> Frame.getCol "ms_run"
                    |> Series.values
                    |> Array.ofSeq
                    |> Array.distinct
                let partitionedNumbers =
                    [|1 .. n|]
                    |> Array.map (sprintf "Assay_%i.json")
                    |> Array.splitInto c
                printfn "Starting random proteomics assay generation"
                [for i in partitionedNumbers do yield async { return (RandomISA.createRandomAssays fileNames min max (i |> Array.map (getRelativePath o)))}]
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore
            with
            | ex -> printfn "%A" ex
        else
            failwith "The given path to the file list is no valid file path"
        printfn "Done"
        0

