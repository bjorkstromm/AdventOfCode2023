
let diffs nums =
    nums
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)

let extrapolate nums =
    let rec loop state (nums : int list) =
        let state = state + nums.[^0]
        let nums = nums |> diffs
        if nums |> List.pairwise |> List.forall (fun (a,b) -> a = b) then
            state + nums.[^0]
        else
            nums |> loop state
    nums |> loop 0

let parse filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line ->
        line.Split ' '
        |> Array.map int
        |> Array.toList)
    |> Array.toList

let part1 filename = 
    filename
    |> parse
    |> List.map extrapolate
    |> List.sum

// Part 2
let part2 filename = 
    filename
    |> parse
    |> List.map (List.rev >> extrapolate)
    |> List.sum