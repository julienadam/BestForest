namespace BestForest

[<AutoOpen>]
module Array2DEx = 

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
