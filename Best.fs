namespace BestForest

open System
open System.IO
open System.Diagnostics
open Support

module Best = 

    /// Implementation of the simulated annealing algo
    let simulatedAnnealing map startingTemp perturbate refreshUI refreshFreq =
        let mutable counter = 0;
        let rec simulatedAnnealingRec map temp score (refreshTimer:Stopwatch) =
            // Refresh the UI
            if refreshTimer.Elapsed > refreshFreq then
                let e = refreshTimer.Elapsed
                refreshUI score temp map (((float counter) / e.TotalSeconds) |> int)
                refreshTimer.Restart()
                counter <- 0

            counter <- counter + 1

            // TODO: store the change and revert it instead of copying the whole array for backup
            let oldMap = map |> Array2D.copy
            let mutable newMap = perturbate map 
            let mutable newScore = newMap |> getTotalScoreForMap
            let delta = newScore - score

            // Compute randomizing factors
            let p = Math.Exp((float delta) / temp)
            let r = rnd.NextDouble()

            if not(delta > 0 || r < p) then
               // Revert the changes
                newMap <- oldMap
                newScore <- score

            // Lower temp
            let newTemp = temp * 0.999999
            if newTemp < 0.01 then
                // Temp has reached minimum, exit
                newMap, newScore
            else
                // Loop again
                simulatedAnnealingRec newMap newTemp newScore refreshTimer

        simulatedAnnealingRec map startingTemp (map |> getTotalScoreForMap) (Stopwatch.StartNew())
