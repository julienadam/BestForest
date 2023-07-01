open BestForest.Support
open BestForest.Best
open System
open System.IO

// ---------------------------------------------------------------------------------------
// Parameters
// ---------------------------------------------------------------------------------------

// Set this close to 1 for maximum variability, and close to 0.01 for minimum variability
// the further away from the minimum the greater changes that would decrease the total score
// are accepted. More variability is good to get out of local maximums. Less is good to avoid
// losing progress.
let initialTemp = 0.2
// UI refresh frequency
let refreshFreq = TimeSpan.FromSeconds(1)
/// Perturbation function for the simulated annealing algorithm
/// Ex: Single random mutation : let perturbate = mutate
/// Ex: Double random mutation : let perturbate = mutate >> mutate
/// Ex: Random 2x2 block mutation : let perturbate = (mutateBloc 2)
let perturbate = mutate

// ---------------------------------------------------------------------------------------

/// Map file name
let fn = __SOURCE_DIRECTORY__ @@ "map.txt"

/// The initial map loaded from the map.txt file, or a random map if not found
let initial = 
    if File.Exists(fn) then
        loadTreeMapFromFile fn
    else
        generateRandomMap terrainSupportMap

open Spectre.Console

// Create the initial layout
let layout = (new Layout("Root")).SplitRows(
    new Layout("Top"), 
    (new Layout("Bottom"))
        .SplitColumns(new Layout("Score"), new Layout("Temperature"), new Layout("Speed")));
layout["Top"].MinimumSize <- initial |> Array2D.length1
let canvas = renderMap initial
layout["Top"].Update(canvas) |> ignore

// Main function
AnsiConsole.Live(layout).Start(fun ctx ->
    let displayProgress score temp map tps =
        updateCanvas map canvas
        layout["Score"].Update(new Text(sprintf "Score : %i" score)) |> ignore
        layout["Temperature"].Update(new Text(sprintf "Temp : %f" temp)) |> ignore
        layout["Speed"].Update(new Text(sprintf "Loops per second : %i" tps)) |> ignore
        File.WriteAllText(fn, map |> formatMap)
        ctx.Refresh()

    let finalMap, finalScore = simulatedAnnealing initial initialTemp perturbate displayProgress refreshFreq
    updateCanvas finalMap canvas
    printfn "Best score %i" finalScore
)