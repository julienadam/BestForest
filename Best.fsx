open System
open System.IO

let loadIntArray filename =
    let lines = File.ReadAllLines(filename)
    lines 
    |> Array.map (fun l -> l.Split(' ') |> Array.map(fun s -> Int32.Parse(s)))
    |> array2D

let root = __SOURCE_DIRECTORY__
let inline (@@) (a:string) (b:string) = Path.Combine(a, b)

let getPath fn = (root @@ "data" @@ "terrain1" @@ fn)

let windMap = loadIntArray (getPath "vent-5-12.txt")
let humMap = loadIntArray (getPath "humidite-5-12.txt")
let sunMap = loadIntArray (getPath "ensoleillement-5-12.txt")

type StepLimits = {
    sun : int
    humidity : int
    wind : int
    growDuration : int
}

type TreeDef = {
    Step0_1 : StepLimits
    Step1_2 : StepLimits
    Step2_3 : StepLimits
    Interactions : Map<string, float>
}

let loadCatalog filename : Map<string, TreeDef> =
    let readStep (line: string) = 
        let split = line.Split(' ') |> Array.map (fun c -> Int32.Parse(c))
        { sun = split.[0]; humidity = split.[1]; wind = split.[2]; growDuration = split.[3] }

    File.ReadAllLines(filename)
    |> Array.chunkBySize 5
    |> Seq.map (fun treeChunks ->
        let name = treeChunks.[0]
        let interactions = 
            treeChunks.[4].Split(' ') 
            |> Seq.skip 1 
            |> Seq.chunkBySize 2 
            |> Seq.map (fun i -> i.[0], Double.Parse(i.[1]))
            |> Map.ofSeq
        
        let treeDesc = { 
            Step0_1 = readStep treeChunks.[1]
            Step1_2 = readStep treeChunks.[2]
            Step2_3 = readStep treeChunks.[3]
            Interactions = interactions
        }
        
        name, treeDesc
        )
    |> Map.ofSeq

loadCatalog (getPath "catalogue-4.txt")