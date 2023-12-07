type Race = {
    Time: uint64
    Distance: uint64
}

let parse (filename : string) =
    let m =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.map (fun line ->
            let line = line.Split(':', 2, System.StringSplitOptions.RemoveEmptyEntries)
            line[1].Split(' ', System.StringSplitOptions.RemoveEmptyEntries))
        |> array2D

    [|0..((Array2D.length2 m) - 1)|]
    |> Array.map (fun i ->
        let time = uint64 m.[0, i]
        let distance = uint64 m.[1, i]
        {
            Time = time
            Distance = distance
        })

let optionsToWin race =
    [|0UL..(race.Time)|]
    |> Array.choose (fun pressed ->
        let speed = pressed
        let travelTime = race.Time - pressed
        let distance = speed * travelTime
        if (distance > race.Distance) then Some pressed else None)

let part1 filename =
    filename
    |> parse
    |> Array.map (optionsToWin >> Array.length)
    |> Array.reduce (*)

// Part 2

let parse2 filename =
    let lines =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.map (fun line ->
            let line = line.Split(':', 2, System.StringSplitOptions.RemoveEmptyEntries)
            line[1].Replace(" ", "") |> uint64)
    {
        Time = lines[0]
        Distance = lines[1]
    }

let part2 filename =
    filename
    |> parse2
    |> optionsToWin
    |> Array.length