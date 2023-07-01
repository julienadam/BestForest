#time
#load "Support.fsx"

open System
open System.IO
open System.Diagnostics
open Support

let refreshFreq = TimeSpan.FromSeconds(1)

let rnd = new Random();

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

let simulatedAnnealing map startingTemp perturbate refreshUI =

    /// Main algo
    let rec simulatedAnnealingRec map temp score (refreshTimer:Stopwatch) =
        // Refresh the UI if necessary
        if refreshTimer.Elapsed > refreshFreq then
            refreshUI score temp map
            refreshTimer.Restart()

        // TODO: store the change and revert it instead of copying the whole array for backup
        let oldMap = map |> Array2D.copy
        let mutable newMap = perturbate map 
        let mutable newScore = newMap |> scoreMap
        let delta = newScore - score

        // Compute randomizing factors
        let p = Math.Exp((float delta) / temp)
        let r = rnd.NextDouble()

        if delta > 0 || r < p then
           // Solution is accepted
           ()
        else
            // Revert the changes
            newMap <- oldMap
            newScore <- score

        // Lower temp
        let newTemp = temp * 0.99999
        if newTemp < 0.01 then
            // Temp has reached minimum, exit
            newMap, newScore
        else
            // Loop again
            simulatedAnnealingRec newMap newTemp newScore refreshTimer

    simulatedAnnealingRec map startingTemp (map |> scoreMap) (Stopwatch.StartNew())

open Spectre.Console

// UI stuff

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

/// Map file name
let fn = __SOURCE_DIRECTORY__ @@ "map.txt"

/// The initial map loaded from the map.txt file, or a random map if not found
let initial = 
    if File.Exists(fn) then
        loadTreeMapFromFile fn
    else
        generateRandomMap terrainSupportMap

let canvas = renderMap initial

// Create the layout
let layout = (new Layout("Root")).SplitRows(
    new Layout("Top"), 
    (new Layout("Bottom")).SplitColumns(new Layout("Score"), new Layout("Temperature")));
layout["Top"].MinimumSize <- initial |> Array2D.length1
layout["Top"].Update(canvas)

// Set this close to 1 for maximum variability, and close to 0.01 for minimum variability
let initialTemp = 0.2

/// Perturbation function for the simulated annealing algorithm

// Single random mutation let perturbate = mutate
// Double random mutation let perturbate = mutate
// Random 2x2 block mutation
let perturbate = (mutateBloc 2)

// Main function
AnsiConsole.Live(layout).Start(fun ctx ->
    let displayProgress score temp map =
        updateCanvas map canvas
        layout["Score"].Update(new Text(sprintf "Score : %i" score)) |> ignore
        layout["Temperature"].Update(new Text(sprintf "Temp : %f" temp)) |> ignore
        File.WriteAllText(fn, map |> formatMap)
        ctx.Refresh()

    let finalMap, finalScore = simulatedAnnealing initial initialTemp perturbate displayProgress
    updateCanvas finalMap canvas
    printfn "Best score %i" finalScore
)
