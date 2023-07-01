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

/// Tries to modify a single random tree until it manages to do so
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

/// Main algo
let rec simulatedAnnealing map temp score (refreshTimer:Stopwatch) perturbate refresh =
    // Refresh the UI if necessary
    if refreshTimer.Elapsed > refreshFreq then
        refresh score temp map
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
        simulatedAnnealing newMap newTemp newScore refreshTimer perturbate refresh

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

/// Perturbation functions for the simulated annealing algorithm
/// Initially set to mutate, could be mutate >> mutate if we 
/// wanted to add more randomness
let perturbate = mutate >> mutate

// Main function
AnsiConsole.Live(layout).Start(fun ctx ->
    let displayProgress score temp map =
        updateCanvas map canvas
        layout["Score"].Update(new Text(sprintf "Score : %i" score)) |> ignore
        layout["Temperature"].Update(new Text(sprintf "Temp : %f" temp)) |> ignore
        File.WriteAllText(fn, map |> formatMap)
        ctx.Refresh()

    let finalMap, finalScore = simulatedAnnealing initial initialTemp (initial |> scoreMap) (Stopwatch.StartNew()) perturbate displayProgress
    updateCanvas finalMap canvas
    printfn "Best score %i" finalScore
)
