#time
#load "Support.fsx"

open System
open System.IO
open System.Linq
open Support

let rnd = new Random();

let rec populate (input:string option[,]) (processingStack:System.Collections.Generic.Stack<int * int>) =
    if processingStack.Count = 0 then
        input |> Array2D.map Option.get
    else
        let i, j = processingStack.Pop()
        if input[i,j].IsSome then
            populate input processingStack
        else
            let candidates = terrainSupportMap[i,j]
            match candidates with
            | [] -> populate input processingStack
            | [a] ->
                Array2D.set input i j (Some a)
                populate input processingStack
            | x ->
                // compute score for all options, keep the best one
                let bestOption = x |> Seq.maxBy (fun candidate ->
                    (Array2D.set input i j (Some candidate))
                    input |> scoreInProgressMap
                )

                Array2D.set input i j (Some bestOption)
                input 
                |> getAdjacent i j 
                |> Seq.filter (fun (r,c,v) -> v.IsNone)
                |> Seq.iter (fun (r,c,v) -> processingStack.Push(r,c))

                populate input processingStack


let initmap () =
    let stack = new System.Collections.Generic.Stack<int*int>()
    stack, terrainSupportMap |> Array2D.mapi(fun r c candidates -> 
        match candidates with
        | [] -> Some "NA"
        | [a] -> Some a
        | _ -> 
            stack.Push(r,c)
            None
    )


let stack, initialMap = initmap()
stack.Push(10,10)

let finalMap = populate initialMap stack
printfn "Score %i" (scoreMap finalMap)

#r "nuget: FSharp.Collections.ParallelSeq"
open FSharp.Collections.ParallelSeq

let generateRandomMap input =
    input |> Array2D.map (fun candidates -> 
        match candidates with
        | [] -> "NA"
        | _ -> candidates.[rnd.Next(0, candidates.Length - 1)]
    )

let generateFavoringASpecificTree tree input =
    input |> Array2D.map (fun candidates -> 
        if candidates |> List.contains tree then
            tree
        else
            match candidates with
            | [] -> "NA"
            | _ -> candidates.[rnd.Next(0, candidates.Length - 1)]
    )


let rec mutate map =
    let i = rnd.Next(0, (map |> Array2D.length1) - 1)
    let j = rnd.Next(0, (map |> Array2D.length2) - 1)
    if terrainSupportMap.[i,j].Length > 1 then
        let existing:string = map.[i,j]
        let others = 
            terrainSupportMap.[i,j] 
            |> List.filter(fun a -> not(a = existing))
        let nextCandidate = others.[rnd.Next(0, others.Length - 1)]
        Array2D.set map i j nextCandidate
        map
    else
        mutate map

let findBest seed iterations = 
    let mutable current = seed
    let mutable bestScore = seed |> scoreMap
    let mutable bestMap = current |> Array2D.copy

    for _ = 0 to iterations do 
        let mutant = current |> mutate
        let score = mutant |> scoreMap
        if score > bestScore then
            bestScore <- score
            bestMap <- mutant |> Array2D.copy
            current <- mutant

    bestScore, bestMap

let outputDir = @"c:\temp\maps"

let loadTreeMapFromFile filename =
    let lines = File.ReadAllLines(outputDir @@ filename)
    lines
    |> Array.map (fun l -> l.Split(' '))
    |> array2D

[1..8]
|> PSeq.iter (fun track ->
    let fn = outputDir @@ (sprintf "%i.txt" track)
    for run = 1 to 1000000 do
        let original = loadTreeMapFromFile fn
        let originalScore = scoreMap original
        let mutable bestFromTrack = Array2D.copy original
        let mutations = rnd.Next(5, 40)
        printfn "Track %i : Starting run %i with %i mutations from score %i" track run mutations originalScore
        for _ = 1 to mutations do
            bestFromTrack <- mutate bestFromTrack

        let afterScore, afterMap = findBest bestFromTrack 3000
        
        if afterScore < originalScore then
            printfn "Track %i : Reverting, score was %i compared to %i" track afterScore originalScore
            File.WriteAllText(fn, original |> formatMap)
        else if originalScore = afterScore then
            printfn "Track %i : No improvement, doing nothing" track
        else if afterScore > originalScore then
            printfn "Track %i : Better score %i compared to previous %i" track afterScore originalScore
            File.WriteAllText(fn, afterMap |> formatMap)
        else
            failwithf "WTF"
)
