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
        let n = results.GetResult NumberDifferentConditions
        let t = results.GetResult TechnicalReplicates
        let map =
            let path = results.TryGetResult TermMap
            match path with
            | Some path -> Some (getPathRelativeToDir path)
            | None      -> None
        let generateMap =
            let path = results.TryGetResult GenerateTermMap
            match path with
            | Some path -> Some (getPathRelativeToDir path)
            | None      -> None
        if Directory.Exists i then
            try
                printfn "Reading filenames"
                let fileNames: string [] =
                    Directory.GetFiles(i, "*.csv")
                    |> Array.map (IO.Path.GetFileNameWithoutExtension)
                printfn "Starting random proteomics assay generation"
                RandomISA.createRandomAssay fileNames n t o map generateMap
            with
            | ex -> printfn "%A" ex
        else
            failwith "The given path to the file list is no valid file path"
        printfn "Done"
        0

