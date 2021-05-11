namespace RandomISA

module RandomISA =
    
    open ISADotNet
    open ISADotNet.XLSX
    open Newtonsoft.Json
    open FSharp.Stats

    type CvParam =
        {
            Name: string
            TermSourceRef: string
            TermAccessionNumber: string
        }
        static member create name ref accNumber =
            {
                Name = name
                TermSourceRef = ref
                TermAccessionNumber = accNumber
            }

    type AttributeValue =
        | Factor
        | Characteristcs
        | Parameter

    type ParameterInfo =
        {
            Type: AttributeValue
            Term: CvParam
            Unit: CvParam option
        }
        static member create typ term unit =
            {
                Type = typ
                Term = term
                Unit = unit
            }

    type CvFromAPI = 
        {
            ID: string
            OntologyID: string
            Accession: string
            Name: string
            Definition: string
            XRefValueType: string
            IsObsolete: string
        }


    let proteomicsSamplePreparation = 
        [
            ParameterInfo.create Characteristcs (CvParam.create "genotype information" "OBI" "OBI:0001305" ) None
            ParameterInfo.create Characteristcs (CvParam.create "sample preparation" "MS" "MS:1000831"     ) None
            ParameterInfo.create Characteristcs (CvParam.create "protein tag" "GO" "GO:0031386"            ) None
            ParameterInfo.create Characteristcs (CvParam.create "biological replicate" "MS" "MS:1001809"   ) None
            ParameterInfo.create Parameter      (CvParam.create "spectrum interpretation" "MS" "MS:1001000") None
            ParameterInfo.create Parameter      (CvParam.create "matrix solution" "MS" "MS:1000834"        ) None
            ParameterInfo.create Parameter      (CvParam.create "temperature" "PATO" "PATO:0000146"        ) (Some (CvParam.create "degree Celsius" "UO" "UO:0000027"))
            ParameterInfo.create Parameter      (CvParam.create "time" "PATO" "PATO:0000165"               ) (Some (CvParam.create "hour" "UO" "UO:0000032"))
        ]

    let proteomicsExtraction = 
        [
            ParameterInfo.create Parameter (CvParam.create "cleavage agent name" "MS" "MS:1001045"     ) None
            ParameterInfo.create Parameter (CvParam.create "molecule" "MS" "MS:1000859"                ) None
            ParameterInfo.create Parameter (CvParam.create "sample state" "MS" "MS:1000003"            ) None
            ParameterInfo.create Parameter (CvParam.create "stainign" "OBI" "OBI:0302887"              ) None
            ParameterInfo.create Parameter (CvParam.create "buffer" "CHEBI" "CHEBI:35225"              ) None
            ParameterInfo.create Parameter (CvParam.create "pH" "UO" "UO:0000196"                      ) (Some (CvParam.create "pH" "UO" "UO:0000196"))
            ParameterInfo.create Parameter (CvParam.create "sample pre-fractionation" "MS" "MS:1002493") None
            ParameterInfo.create Parameter (CvParam.create "protein column" "OBI" "OBI:0000468"        ) None
        ]

    let proteomicsMeasurement = 
        [
            ParameterInfo.create Parameter (CvParam.create "technical replicate" "MS" "MS:1001808") None
            ParameterInfo.create Parameter (CvParam.create "sample volume" "MS" "MS:1000005"      ) (Some (CvParam.create "microliter" "UO" "UO:0000101"))
            ParameterInfo.create Parameter (CvParam.create "injection volume" "" ""               ) (Some (CvParam.create "microliter" "UO" "UO:0000101"))
            ParameterInfo.create Parameter (CvParam.create "count unit" "UO" "UO:0000189"         ) (Some (CvParam.create "count unit" "UO" "UO:0000189"))
            ParameterInfo.create Parameter (CvParam.create "instrument model" "MS" "MS:1000031"   ) None
            ParameterInfo.create Parameter (CvParam.create "duration" "PATO" "PATO:0001309"       ) (Some (CvParam.create "minute" "UO" "UO:0000031"))
        ]

    let proteomicsDataProcessing = 
        [
            ParameterInfo.create Parameter (CvParam.create "acquisition software" "MS" "MS:1001455"    ) None
            ParameterInfo.create Parameter (CvParam.create "analysis software" "MS" "MS:1001456"       ) None
            ParameterInfo.create Parameter (CvParam.create "data processing software" "MS" "MS:1001457") None
        ]

    let createParameter (parameterInfo: ParameterInfo) =
        ProtocolParameter.fromString parameterInfo.Term.Name parameterInfo.Term.TermSourceRef parameterInfo.Term.TermAccessionNumber
    
    let createParameterValues (parameter: ProtocolParameter) (value: Value option) (unitOntology: OntologyAnnotation option)=
            ProcessParameterValue.create  (Some parameter) value unitOntology

    let updateParameterValue (name: string) (value: Value option) (paramValues: ProcessParameterValue list) =
        paramValues
        |> List.map (fun x ->
            if x.Category.Value.NameAsString = name then
                {x with Value = value}
            else x
        )

    let createProtocol (parameters: ProtocolParameter list) =
        Protocol.create None None None None None None (Some parameters) None None
    
    let getAllChildTerms (param: ParameterInfo) =
        let request = sprintf """[{"Name": "%s","TermAccession": "%s"}]""" param.Term.Name param.Term.TermAccessionNumber
        FSharp.Data.Http.RequestString(
            "https://swate.denbi.uni-tuebingen.de/api/IAnnotatorAPIv1/getAllTermsByParentTerm",
            httpMethod = "POST",
            body = FSharp.Data.TextRequest request
        )
        |> JsonConvert.DeserializeObject<CvFromAPI []>

    let getRandomChildTerm (cvs: CvFromAPI []) =
        cvs
        |> Array.shuffleFisherYates
        |> Array.tryHead
        |> fun term ->
            match term with
            | Some x -> Value.fromOptions (Some x.Name) (Some x.Accession) (Some (x.Accession.Split(':') |> Array.head))
            | None -> None

    let childTermMap =
        List.concat[proteomicsSamplePreparation; proteomicsExtraction; proteomicsMeasurement; proteomicsDataProcessing]
        |> List.map (fun x ->
            x,
            x |> getAllChildTerms
        )
        |> Map.ofList
    
    let createRandomValues (paramInfos: ParameterInfo list) =
        let random = new System.Random()
        let getRandomWithRange min max=
            random.NextDouble() * (max - min) + min
            |> string
        let getRandomFromArray (arr: 'a [])=
            arr
            |> Array.shuffleFisherYates
            |> Array.head
            |> string
        let factor,rest =
            match random.Next(0,5) with
            | 0 ->
                let rec loop (param: ParameterInfo) =
                    match param.Term.Name with
                    | "count unit" | "biological replicate" | "technical replicate" -> 
                        paramInfos
                        |> Array.ofList
                        |> Array.shuffleFisherYates
                        |> Array.head
                        |> loop
                    | _ -> param
                let factor = loop (paramInfos |> Array.ofList |> Array.shuffleFisherYates |> Array.head)
                [factor],(paramInfos |> List.filter (fun x -> x <> factor))
            | _ -> [],paramInfos
        let protocol,parameterValues =
            rest
            |> List.filter (fun x -> x.Type = Parameter)
            |> List.map (fun pInfo ->
                match pInfo.Unit with
                | Some unit -> 
                    match pInfo.Term.Name with
                    | "temperature" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some (random.Next(10, 50) |> string)) (None) (None))
                            (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                    | "pH" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some (getRandomWithRange 5. 9.)) (None) (None))
                            (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                    | "time" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some (getRandomFromArray [|0.5;1.;2.;4.;8.;24.;48.|])) (None) (None))
                            (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                    | "sample volume" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some (random.Next(100,500) |> string)) (None) (None))
                            (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                    | "injection volume" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some (getRandomWithRange 1. 40.)) (None) (None))
                            (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                    | "count unit" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some "1") (None) (None))
                            (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                    | "duration" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some (random.Next(20,480) |> string)) (None) (None))
                            (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                | None ->
                    match pInfo.Term.Name with
                    | "technical replicate" ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (Value.fromOptions (Some "1") (None) (None))
                            None
                    | _ ->
                        (createParameter pInfo),
                        createParameterValues
                            (createParameter pInfo)
                            (childTermMap.[pInfo] |> getRandomChildTerm)
                            None
            )
            |> List.unzip
            |> fun (parameters, paramValue) ->
                createProtocol parameters, paramValue
        let characteristicValues =
            rest
            |> List.filter (fun x -> x.Type = Characteristcs)
            |> List.map (fun pInfo ->
                match pInfo.Unit with
                | Some unit -> failwith (sprintf "%A not expected in Characteristics" unit)
                | None ->
                    match pInfo.Term.Name with
                    | "biological replicate" ->
                        MaterialAttributeValue.create
                            None
                            (Some (MaterialAttribute.fromString pInfo.Term.Name pInfo.Term.TermAccessionNumber pInfo.Term.TermSourceRef))
                            (Value.fromOptions (Some "1") (None) (None))
                            None
                    | _ ->
                        MaterialAttributeValue.create
                            None
                            (Some (MaterialAttribute.fromString pInfo.Term.Name pInfo.Term.TermAccessionNumber pInfo.Term.TermSourceRef))
                            (childTermMap.[pInfo] |> getRandomChildTerm)
                            None
            )
        let factorValues =
            if factor.IsEmpty then None
            else
                let factor' = (Some (Factor.fromString factor.Head.Term.Name factor.Head.Term.Name factor.Head.Term.TermAccessionNumber factor.Head.Term.TermSourceRef))
                let values =
                    [1 .. 100]
                    |> List.map (fun _ ->
                        match factor.Head.Unit with
                        | Some unit -> 
                            match factor.Head.Term.Name with
                            | "temperature" ->
                                (Value.fromOptions (Some (getRandomWithRange 10. 50.)) (None) (None)),
                                (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                            | "pH" ->
                                (Value.fromOptions (Some (getRandomWithRange 5. 9.)) (None) (None)),
                                (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                            | "time" ->
                                (Value.fromOptions (Some (getRandomFromArray [|0.5;1.;2.;4.;8.;24.;48.|])) (None) (None)),
                                (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                            | "sample volume" ->
                                (Value.fromOptions (Some (random.Next(100,500) |> string)) (None) (None)),
                                (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                            | "injection volume" ->
                                (Value.fromOptions (Some (getRandomWithRange 1. 40.)) (None) (None)),
                                (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                            | "count unit" ->
                                (Value.fromOptions (Some "1") (None) (None)),
                                (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                            | "duration" ->
                                (Value.fromOptions (Some (random.Next(20,480) |> string)) (None) (None)),
                                (Some (OntologyAnnotation.fromString unit.Name unit.TermAccessionNumber unit.TermSourceRef))
                        | None ->
                            (childTermMap.[factor.Head] |> getRandomChildTerm),
                            None
                    )
                let factorVals =
                    values
                    |> List.map (fun (valueOpt, ontAnnoOpt) ->
                        FactorValue.create None factor' valueOpt ontAnnoOpt
                    )
                Some factorVals
                
        protocol, parameterValues, characteristicValues, factorValues

    let createRandomAssay (files: string []) (min: int) (max: int) (technicalReplicates: int) path =
        let random = new System.Random()
        let partitionedFiles =
            files
            |> Array.shuffleFisherYates
            |> Array.chunkBySize technicalReplicates
            |> Array.filter (fun x -> x.Length = technicalReplicates)
            |> Array.splitInto (random.Next(min,max))
        partitionedFiles
        |> Array.map (fun set ->
            let samplePrepProtocol, samplePrepParamVal, samplePrepCharVal, samplePrepFactorVal =
                createRandomValues proteomicsSamplePreparation
            let extractionProtocol, extractionParamVal, extractionCharVal, extractionFactorVal =
                createRandomValues proteomicsExtraction
            let measurementProtocol, measurementParamVal, measurementCharVal, measurementFactorVal =
                createRandomValues proteomicsMeasurement
            let dataProcessingProtocol, dataProcessingParamVal, dataProcessingCharVal, dataProcessingFactorVal =
                createRandomValues proteomicsDataProcessing
            let samplePrepProcess =
                let input,output =
                    [|1 .. set.Length|]
                    |> Array.map (fun i ->
                        let factorVal =
                            match samplePrepFactorVal with
                            | Some value -> Some [value.[i-1]]
                            | None -> None
                        let processInput =
                            ProcessInput.Sample (Sample.create None (Some (sprintf "SamplePrepIn_%i" i)) (Some samplePrepCharVal) None None)
                        let processOutput =
                            ProcessOutput.Sample (Sample.create None (Some (sprintf "SamplePrepOut_ExtractionIn_%i" i)) (Some samplePrepCharVal) factorVal None)
                        processInput, processOutput
                    )
                    |> List.ofArray
                    |> List.unzip
                Process.create None None (Some samplePrepProtocol) (Some samplePrepParamVal) None None None None (Some input) (Some output) None
            let extractionProcess =
                let input,output =
                    [|1 .. set.Length|]
                    |> Array.map (fun i ->
                        let factorVal =
                            match extractionFactorVal with
                            | Some value -> Some [value.[i-1]]
                            | None -> None
                        let processInput =
                            ProcessInput.Sample (Sample.create None (Some (sprintf "SamplePrepOut_ExtractionIn_%i" i)) (Some extractionCharVal) None None)
                        let processOutput =
                            ProcessOutput.Sample (Sample.create None (Some (sprintf "ExtractionOut_MeasurementIn_%i" i)) (Some extractionCharVal) factorVal None)
                        processInput, processOutput
                    )
                    |> List.ofArray
                    |> List.unzip
                Process.create None None (Some extractionProtocol) (Some extractionParamVal) None None None None (Some input) (Some output) None
            let measurementProcess =
                let inputs =
                    [|1 .. set.Length|]
                    |> Array.map (fun i ->
                        let factorVal =
                            match measurementFactorVal with
                            | Some value -> Some [value.[i-1]]
                            | None -> None
                        let processInput =
                            ProcessInput.Sample (Sample.create None (Some (sprintf "ExtractionOut_MeasurementIn_%i" i)) (Some measurementCharVal) None None)
                        processInput, factorVal
                    )
                let inputOutput =
                    [|1 ..  technicalReplicates|]
                    |> Array.map (fun i ->
                        inputs
                        |> Array.mapi (fun j (input,factorVal) ->
                            input,
                            ProcessOutput.Sample (Sample.create None (Some (sprintf "MeasurementOut_ProcessingIn_%i" ((i-1)*inputs.Length+j+1))) (Some measurementCharVal) factorVal None)
                        )
                        |> List.ofArray
                        |> List.unzip
                    )
                inputOutput
                |> Array.mapi (fun i (input,output) ->
                    let newMeasurementParamVal =
                        measurementParamVal
                        |> updateParameterValue "technical replicate" (Value.fromOptions (Some (string(i+1))) (None) (None))
                    Process.create None None (Some measurementProtocol) (Some newMeasurementParamVal) None None None None (Some input) (Some output) None
                )
                |> List.ofArray
            let dataProcessingProcess =
                let input,output =
                    set
                    |> Array.concat
                    |> Array.mapi (fun i file->
                        let factorVal =
                            match dataProcessingFactorVal with
                            | Some value -> Some [value.[i]]
                            | None -> None
                        let processInput =
                            ProcessInput.Sample (Sample.create None (Some (sprintf "MeasurementOut_ProcessingIn_%i" (i+1))) (Some dataProcessingCharVal) None None)
                        let processOutput =
                            ProcessOutput.Sample (Sample.create None (Some file) (Some dataProcessingCharVal) factorVal None)
                        processInput, processOutput
                    )
                    |> List.ofArray
                    |> List.unzip
                Process.create None None (Some dataProcessingProtocol) (Some dataProcessingParamVal) None None None None (Some input) (Some output) None
            let updatedProcesses =
                [[samplePrepProcess];[extractionProcess];measurementProcess;[dataProcessingProcess]]
                |> List.concat
                |> ISADotNet.XLSX.AssayFile.AnnotationTable.updateSamplesByThemselves
                |> List.ofSeq
            updatedProcesses
        )
        |> List.ofArray
        |> List.concat
        |> fun processes -> 
            Assay.create None None None None None None None None None (Some processes) None
            |> Json.Assay.toFile path