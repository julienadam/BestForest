#r "nuget: Spectre.Console"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open System
open FSharp.Collections.ParallelSeq

let rnd = new Random();

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

let getScoreForCell trees i j treeType =
    if treeType = "NA" then 
        0
    else
        let surroundingTrees = trees |> getAdjacent i j
        let treeDef = catalog.[treeType]
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

let getScoreMap (trees:string[,]) = trees |> Array2D.mapi (getScoreForCell trees)

let getTotalScoreForMap (trees:string[,]) =
    trees |> enumArray2d |> PSeq.sumBy (fun (i,j,treeType) -> getScoreForCell trees i j treeType)

let formatMap (map:string[,] ) =
    let sb = new System.Text.StringBuilder()
    for i = 0 to (map |> Array2D.length1) - 1 do
        for j = 0 to (map |> Array2D.length2) - 1 do
            sb.Append(map[i,j]) |> ignore
            if j <> (map |> Array2D.length2) - 1 then 
                sb.Append(" ") |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()

let loadTreeMapFromFile filename =
    File.ReadAllLines(filename)
    |> Array.map (fun l -> l.Split(' '))
    |> array2D

open Spectre.Console

/// Display the map in a Spectre canvas
let updateCanvas map (canvas:Canvas) =
    let scores = getScoreMap map
    let colorMatcher = function
        | 0 -> Color.LightSkyBlue1
        | 1 -> Color.Orange1
        | 2 -> Color.LightYellow3
        | 4 -> Color.Green
        | x -> failwithf "Not a valid score %i" x
    scores |> Array2D.iteri(fun i j v ->
        match map[i,j] with
        | "NA" -> canvas.SetPixel(j, i, Color.Grey) |> ignore
        | _ -> canvas.SetPixel(j, i, colorMatcher v) |> ignore
    )

let renderMap (map:string[,] ) =
    let canvas = new Canvas(map |> Array2D.length2, map |> Array2D.length1)
    updateCanvas map canvas
    canvas

/// Initialize a new random map from a known terrain
let generateRandomMap input =
    input |> Array2D.map (fun candidates -> 
        match candidates with
        | [] -> "NA"
        | _ -> candidates.[rnd.Next(0, candidates.Length - 1)]
    )

/// Modifies a single cell at random (if possible)
let mutateCell (map:string[,]) i j =
    if terrainSupportMap.[i,j].Length > 1 then
        let existing:string = map.[i,j]
        let others = 
            terrainSupportMap.[i,j] 
            |> List.filter(fun a -> not(a = existing))
        let nextCandidate = others.[rnd.Next(0, others.Length - 1)]
        Array2D.set map i j nextCandidate
        true
    else
        false

/// Tries to modify a single random tree until it manages to do so
let rec mutate map =
    let i = rnd.Next(0, (map |> Array2D.length1) - 1)
    let j = rnd.Next(0, (map |> Array2D.length2) - 1)
    if mutateCell map i j then
        map
    else
        mutate map

/// Tries to mutate a block at random, using the provided block size
let mutateBloc blockSize map =
    let i0 = rnd.Next(0, (map |> Array2D.length1) - 1 - (blockSize - 1))
    let j0 = rnd.Next(0, (map |> Array2D.length2) - 1 - (blockSize - 1))
    for i = 0 to blockSize - 1 do
        for j = 0 to blockSize - 1 do
            mutateCell map (i0 + i) (j0 + j) |> ignore
    map