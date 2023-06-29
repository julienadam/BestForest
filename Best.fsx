#time
#r "nuget: FSharp.Collections.ParallelSeq"

open FSharp.Collections.ParallelSeq
open System
open System.IO

type Terrain = | T1 | T2

let terrain = T2

let loadIntArray filename =
    let lines = File.ReadAllLines(filename)
    lines 
    |> Array.map (fun l -> l.Split(' ') |> Array.map(fun s -> Int32.Parse(s)))
    |> array2D

let root = __SOURCE_DIRECTORY__
let inline (@@) (a:string) (b:string) = Path.Combine(a, b)

type Cell = {
    sun : int
    humidity : int
    wind : int
}

type StepLimits = {
    sun : int
    humidity : int
    wind : int
    growDuration : int
}
with 
    member x.IsSupportedOn(cell:Cell) =
        cell.humidity   >= x.humidity && 
        cell.sun        >= x.sun && 
        cell.wind       >= x.wind

type TreeDef = {
    Step0_1 : StepLimits
    Step1_2 : StepLimits
    Step2_3 : StepLimits
    Interactions : Map<string, float>
}
with 
    member x.IsSupportedOn(cell:Cell) =
        x.Step0_1.IsSupportedOn(cell)

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


let LoadTerrain1 () = 
    let getPath fn = (root @@ "data" @@ "terrain1" @@ fn)
    let windMap = loadIntArray (getPath "vent-5-12.txt")
    let humMap = loadIntArray (getPath "humidite-5-12.txt")
    let sunMap = loadIntArray (getPath "ensoleillement-5-12.txt")
    let cat = loadCatalog (getPath "catalogue-4.txt")
    let biome = Array2D.init (Array2D.length1 windMap) (Array2D.length2 windMap) (fun x y -> { sun = sunMap.[x,y]; humidity = humMap.[x,y]; wind = windMap.[x,y]})
    biome, cat

let LoadTerrain2 () = 
    let getPath fn = (root @@ "data" @@ "terrain2" @@ fn)
    let windMap = loadIntArray (getPath "vent-50-100.txt")
    let humMap = loadIntArray (getPath "humidite-50-100.txt")
    let sunMap = loadIntArray (getPath "ensoleillement-50-100.txt")
    let cat = loadCatalog (getPath "catalogue-17.txt")
    let biome = Array2D.init (Array2D.length1 windMap) (Array2D.length2 windMap) (fun x y -> { sun = sunMap.[x,y]; humidity = humMap.[x,y]; wind = windMap.[x,y]})
    biome, cat

let terrainBiome, catalog = 
    match terrain with 
    | T1 -> LoadTerrain1 ()
    | T2 -> LoadTerrain2 ()

let terrainSupportMap =
    terrainBiome |> Array2D.map(
        fun c -> 
            catalog 
            |> Map.toSeq 
            |> Seq.filter (fun (_, t) -> t.IsSupportedOn(c))
            |> Seq.map fst
            |> Seq.toList
    )

let getAdjacent row col (grid:'T[,]) = seq {
    if row > 0 then
        yield ((row - 1), col, grid.[(row - 1), col])
    if row < ((grid |> Array2D.length1) - 1) then
        yield ((row + 1), col, grid.[(row + 1), col])
    if col > 0 then
        yield (row, (col - 1), grid.[row, (col - 1)])
    if col < ((grid |> Array2D.length2) - 1) then
        yield (row, (col + 1), grid.[row, (col + 1)])
    if row > 0 && col > 0 then
        yield ((row - 1), (col - 1), grid.[(row - 1), (col - 1)])
    if row > 0 && col < ((grid |> Array2D.length2) - 1) then
        yield ((row - 1), (col + 1), grid.[(row - 1), (col + 1)])
    if row <((grid |> Array2D.length1) - 1) && col > 0 then
        yield ((row + 1), (col - 1), grid.[(row + 1), (col - 1)])
    if row <((grid |> Array2D.length1) - 1) && col < ((grid |> Array2D.length2) - 1) then
        yield ((row + 1), (col + 1), grid.[(row + 1), (col + 1)])
}

let enumArray2d (array:'a[,]) = seq {
    for i = 0 to (array |> Array2D.length1) - 1 do
        for j = 0 to (array |> Array2D.length2) - 1 do
            yield i,j, array.[i,j]
}

let scoreMap (trees:string[,]) =
    trees 
    |> enumArray2d
    |> Seq.map (fun (i, j, t) -> 
        if t = "NA" then 
            0
        else
            let surroundingTrees = trees |> getAdjacent i j
            let treeDef = catalog.[t]
            let growthBonus = 
                surroundingTrees 
                |> Seq.sumBy(fun (_,_,neighbor) -> 
                    match treeDef.Interactions |> Map.tryFind neighbor with
                    | Some bonus -> bonus
                    | _ -> 0.0)

            let normalisedBonus = Math.Max(0.0, 1.0 + growthBonus)
            let biome = terrainBiome.[i,j]
            if treeDef.Step0_1.IsSupportedOn(biome) then
                let growthTime = Math.Ceiling((treeDef.Step0_1.growDuration |> float) * normalisedBonus) |> int
                let finalGrowthTime = match growthTime with | 0 -> 1 | x -> x
                let remainingForStep2 = 60 - finalGrowthTime
                if remainingForStep2 < 0 then
                    0
                else if treeDef.Step1_2.IsSupportedOn(biome) then
                    let growthTime = Math.Ceiling((treeDef.Step1_2.growDuration |> float) * normalisedBonus) |> int
                    let finalGrowthTime = match growthTime with | 0 -> 1 | x -> x
                    let remainingForStep3 = remainingForStep2 - finalGrowthTime
                    if remainingForStep3 < 0 then
                        1
                    else if treeDef.Step2_3.IsSupportedOn(biome) then
                        let growthTime = Math.Ceiling((treeDef.Step2_3.growDuration |> float) * normalisedBonus) |> int
                        let finalGrowthTime = match growthTime with | 0 -> 1 | x -> x
                        if remainingForStep3 - finalGrowthTime >= 0 then
                            4
                        else
                            2
                    else
                        2
                else
                    1
            else
                0
    )
    |> Seq.sum

let rnd = new Random();

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

let formatMap (map:string[,] ) =
    let sb = new System.Text.StringBuilder()
    for i = 0 to (map |> Array2D.length1) - 1 do
        for j = 0 to (map |> Array2D.length2) - 1 do
            sb.Append(map[i,j]) |> ignore
            if j <> (map |> Array2D.length2) - 1 then 
                sb.Append(" ") |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()

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

let findBest seed iterations track outputDir = 
    let mutable current = seed
    let mutable bestScore = seed |> scoreMap

    for iteration = 0 to iterations do 
        let mutant = current |> mutate
        let score = mutant |> scoreMap
        if score > bestScore then
            printfn "found best map with score : %i on track %i at iteration %i" score track iteration
            let fn = outputDir @@ (sprintf "%i.txt" track)
            File.WriteAllText(fn, mutant |> formatMap)
            bestScore <- score
            current <- current

    bestScore

let outputDir = @"c:\temp\maps"

let loadTreeMapFromFile filename =
    let lines = File.ReadAllLines(outputDir @@ filename)
    lines
    |> Array.map (fun l -> l.Split(' '))
    |> array2D

let loadAndMassMutate track mutations =
    let fn = outputDir @@ (sprintf "%i.txt" track)
    File.Copy(fn, Path.ChangeExtension(fn, ".bak"), true) |> ignore
    let mutable map = loadTreeMapFromFile fn
    for _ = 1 to mutations do
        map <- mutate map
    map

[1..8]
|> PSeq.iter (fun i ->
    for _ = 1 to 10000 do
        let bestFromTrack = loadAndMassMutate i 10
        let before = scoreMap bestFromTrack
        let after = findBest bestFromTrack 3000 i outputDir
        printfn "Track %i : Best score after 3000 mutations : %i compared to previous %i" i after before

        if before > after then
            printfn "Reverting"
            let fn = outputDir @@ (sprintf "%i.txt" i)
            File.Copy(fn, Path.ChangeExtension(fn, ".txt"), true) |> ignore
)
