namespace BestForest

open System.IO
open System
open FSharp.Collections.ParallelSeq

module Support = 

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
        Id : int
        Code : string
        Step0_1 : StepLimits
        Step1_2 : StepLimits
        Step2_3 : StepLimits
    }
    with 
        member x.IsSupportedOn(cell:Cell) =
            x.Step0_1.IsSupportedOn(cell)

    let loadCatalog filename : float[,] * TreeDef[] =
        let readStep (line: string) = 
            let split = line.Split(' ') |> Array.map (fun c -> Int32.Parse(c))
            { sun = split.[0]; humidity = split.[1]; wind = split.[2]; growDuration = split.[3] }

        // Load the tree codes in a map, the key is the index of the tree in the catalog
        // The idea is to manipulate ints instead of strings to speed up processing
        let codeMapping = 
            File.ReadAllLines(filename)
            |> Array.chunkBySize 5
            |> Seq.mapi (fun idx chunk -> (chunk |> Seq.head), idx)
            |> Map.ofSeq

        // Build an interaction matrix, so that we can easily find the value without 
        // going through a hashtable
        let interactionsMatrix = Array2D.zeroCreate codeMapping.Count codeMapping.Count

        let treeDefinitions = 
            File.ReadAllLines(filename)
            |> Array.chunkBySize 5
            |> Seq.mapi (fun idx treeChunks ->
                // Fill the interaction matrix for this tree
                treeChunks.[4].Split(' ')
                |> Seq.skip 1 
                |> Seq.chunkBySize 2 
                |> Seq.iter (fun i ->
                    let otherTreeId = codeMapping.[i.[0]]
                    Array2D.set interactionsMatrix idx otherTreeId (Double.Parse(i.[1])))

                {
                    Id = idx
                    Code = treeChunks.[0]
                    Step0_1 = readStep treeChunks.[1]
                    Step1_2 = readStep treeChunks.[2]
                    Step2_3 = readStep treeChunks.[3]
                })
            |> Seq.toArray

        interactionsMatrix, treeDefinitions

    let LoadTerrain1 () = 
        let getPath fn = (root @@ "data" @@ "terrain1" @@ fn)
        let windMap = loadIntArray (getPath "vent-5-12.txt")
        let humMap = loadIntArray (getPath "humidite-5-12.txt")
        let sunMap = loadIntArray (getPath "ensoleillement-5-12.txt")
        let interactionMatrix, catalog = loadCatalog (getPath "catalogue-4.txt")
        let biome = Array2D.init (Array2D.length1 windMap) (Array2D.length2 windMap) (fun x y -> { sun = sunMap.[x,y]; humidity = humMap.[x,y]; wind = windMap.[x,y]})
        biome, interactionMatrix, catalog

    let LoadTerrain2 () = 
        let getPath fn = (root @@ "data" @@ "terrain2" @@ fn)
        let windMap = loadIntArray (getPath "vent-50-100.txt")
        let humMap = loadIntArray (getPath "humidite-50-100.txt")
        let sunMap = loadIntArray (getPath "ensoleillement-50-100.txt")
        let interactionMatrix, catalog = loadCatalog (getPath "catalogue-17.txt")
        let biome = Array2D.init (Array2D.length1 windMap) (Array2D.length2 windMap) (fun x y -> { sun = sunMap.[x,y]; humidity = humMap.[x,y]; wind = windMap.[x,y]})
        biome, interactionMatrix, catalog

    let terrainBiome, interactionMatrix, catalog = 
        match terrain with 
        | T1 -> LoadTerrain1 ()
        | T2 -> LoadTerrain2 ()

    let terrainSupportMap =
        terrainBiome |> Array2D.map(
            fun c -> 
                catalog 
                |> Seq.filter (fun t -> t.IsSupportedOn(c))
                |> Seq.toList
        )

    // Build a static list of adjacent cells to avoid recalculating them all the time
    let adjacencyMap = 
        terrainSupportMap |> Array2D.mapi (fun i j _ ->
            terrainSupportMap |> getAdjacent i j |> Seq.map (fun (ni, nj, _ ) -> (ni, nj)) |> Seq.toArray
        )

    let getScoreForCell (trees:int[,]) i j treeType =
        if treeType = -1 then 
            0
        else
            let treeDef = catalog.[treeType]
            let growthBonus = 
                adjacencyMap[i,j]
                |> Seq.sumBy(fun (i,j) -> 
                    let neighbor = trees.[i,j]
                    if neighbor = -1 then 0.0 else interactionMatrix[treeType, neighbor])

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

    let getScoreMap (trees:int[,]) = trees |> Array2D.mapi (getScoreForCell trees)

    let getTotalScoreForMap (trees:int[,]) =
        trees |> enumArray2d |> PSeq.sumBy (fun (i,j,treeType) -> getScoreForCell trees i j treeType)

    let formatMap (map:int[,] ) =
        let sb = new System.Text.StringBuilder()
        for i = 0 to (map |> Array2D.length1) - 1 do
            for j = 0 to (map |> Array2D.length2) - 1 do
                let code = match map[i,j] with | -1 -> "NA" | x -> catalog[x].Code
                sb.Append(code) |> ignore
                if j <> (map |> Array2D.length2) - 1 then 
                    sb.Append(" ") |> ignore
            sb.AppendLine() |> ignore
        sb.ToString()

    let loadTreeMapFromFile filename =
        File.ReadAllLines(filename)
        |> Array.map (fun l -> l.Split(' '))
        |> array2D
        |> Array2D.map (fun v -> 
            match v with | "NA" -> -1 | x -> (catalog |> Seq.find (fun c -> c.Code = x)).Id)

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
            | -1 -> canvas.SetPixel(j, i, Color.Grey) |> ignore
            | _ -> canvas.SetPixel(j, i, colorMatcher v) |> ignore
        )

    let renderMap (map:int[,] ) =
        let canvas = new Canvas(map |> Array2D.length2, map |> Array2D.length1)
        updateCanvas map canvas
        canvas

    /// Initialize a new random map from a known terrain
    let generateRandomMap input =
        input |> Array2D.map (fun candidates -> 
            match candidates with
            | [] -> -1
            | _ -> (candidates.[rnd.Next(0, candidates.Length - 1)]).Id
        )
        
    // Build a list of mutable cells and the available options
    let mutableCells = 
        terrainSupportMap 
        |> enumArray2d
        |> Seq.filter(fun (_,_,treeDefs) -> treeDefs.Length > 1)
        |> Seq.map(fun (i,j,treeDefs) ->i, j, treeDefs |> List.map (fun treeDef -> treeDef.Id) |> List.toArray)
        |> Seq.toArray

    /// Mutates a single cell, based on the precalculated list of mutable cells
    let rec mutate (map: int[,]) =
        let (i, j, allVariants) = mutableCells[rnd.Next(0, mutableCells.Length - 1)]
        let existing = map.[i,j]
        let others = allVariants |> Array.filter(fun a -> not(a = existing))
        let selected = match others with | [|a|] -> a | _ -> others[rnd.Next(0, others.Length - 1)]
        Array2D.set map i j selected
        map

    /// Modifies a single cell at random (if possible)
    let mutateCell (map:int[,]) i j =
        if terrainSupportMap.[i,j].Length > 1 then
            let existing = map.[i,j]
            let others = 
                terrainSupportMap.[i,j] 
                |> List.filter(fun a -> not(a.Id = existing))
            let nextCandidate = others.[rnd.Next(0, others.Length - 1)]
            Array2D.set map i j nextCandidate.Id
            true
        else
            false

    /// Tries to mutate a block at random, using the provided block size
    let mutateBloc blockSize map =
        let i0 = rnd.Next(0, (map |> Array2D.length1) - 1 - (blockSize - 1))
        let j0 = rnd.Next(0, (map |> Array2D.length2) - 1 - (blockSize - 1))
        for i = 0 to blockSize - 1 do
            for j = 0 to blockSize - 1 do
                mutateCell map (i0 + i) (j0 + j) |> ignore
        map