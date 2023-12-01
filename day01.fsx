
let firstNumber xs =
    xs
    |> Seq.skipWhile (fun x -> not (System.Char.IsDigit x))
    |> Seq.head
    |> System.Char.GetNumericValue
    |> int

let lastNumber xs =
    xs
    |> Seq.rev
    |> firstNumber

let calibrationValue (input : string) =
    let arr = input.ToCharArray()
    let firstNum = firstNumber arr
    let lastNum = lastNumber arr
    firstNum * 10 + lastNum

let part1 fileName =
    fileName
    |> System.IO.File.ReadAllLines
    |> Array.map calibrationValue
    |> Array.sum

// Part 2

let digits = [|
    ("1",       1)
    ("2",       2)
    ("3",       3)
    ("4",       4)
    ("5",       5)
    ("6",       6)
    ("7",       7)
    ("8",       8)
    ("9",       9)
    ("one",     1)
    ("two",     2)
    ("three",   3)
    ("four",    4)
    ("five",    5)
    ("six",     6)
    ("seven",   7)
    ("eight",   8)
    ("nine",    9)
|]

let calibrationValue2 (input : string) =
    let digitByIndex =
        digits
        |> Array.choose (fun (c, num) ->
            match input.IndexOf c with
            | -1 -> None
            | i -> Some (i, num))

    let firstNum = digitByIndex |> Array.minBy fst |> snd
    let lastNum = digitByIndex |> Array.maxBy fst |> snd
    firstNum * 10 + lastNum

let part2 fileName =
    fileName
    |> System.IO.File.ReadAllLines
    |> Array.map calibrationValue2
    |> Array.sum