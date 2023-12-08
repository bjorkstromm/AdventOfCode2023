open System.Text.RegularExpressions
type Node = {
    Name: string
    Left: string
    Right: string
}

type Instruction =
    | Left
    | Right

module Instructions =
    let next (instructions : Instruction list) =
        match instructions with
        | [] -> failwithf "Empty instructions %A" instructions
        | head::tail -> (head, tail @ [head])

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Instructions|_|) str =
    match str with
    | ParseRegex @"^([LR]+)$" [instructions] ->
        instructions
        |> Seq.map (function
                    | 'L' -> Left
                    | 'R' -> Right 
                    | _ -> failwithf "Invalid instruction %A" str)
        |> List.ofSeq
        |> Some
    | _ -> None

let (|Node|_|) str =
    match str with
    | ParseRegex @"^([0-9A-Z]+) = \(([0-9A-Z]+), ([0-9A-Z]+)\)$" [name; left; right] ->
        { Name = name; Left = left; Right = right } |> Some
    | _ -> None

let parse (filename : string) =
    let instructions, nodes =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.fold (fun (instructions, nodes) line ->
            match line with
            | Node n -> (instructions, n::nodes)
            | Instructions i -> (i, nodes)
            | _ -> (instructions, nodes))
            ([], [])

    let nodeMap =
        nodes
        |> List.map (fun n -> (n.Name, n))
        |> Map.ofList

    (instructions, nodeMap)

let traverse (start, stop) (instructions, nodeMap : Map<string, Node>) =
    let rec traverse' (node : Node) (instructions : Instruction list) (steps) =
        if node.Name = stop then steps
        else
            let steps = steps + 1
            let (next, instructions) = Instructions.next instructions
            match next with
            | Left -> traverse' (nodeMap.[node.Left]) instructions steps
            | Right -> traverse' (nodeMap.[node.Right]) instructions steps

    traverse' (nodeMap.[start]) instructions 0

let part1 filename =
    filename
    |> parse
    |> traverse ("AAA", "ZZZ")

// Part 2
let traverse2 (startChar, stopChar) (instructions, nodeMap : Map<string, Node>) =
    let rec traverse' (node : Node) (instructions : Instruction list) (steps) =
        if node.Name.[2] = stopChar then steps
        else
            let steps = steps + 1UL
            let (next, instructions) = Instructions.next instructions
            match next with
            | Left -> traverse' (nodeMap.[node.Left]) instructions steps
            | Right -> traverse' (nodeMap.[node.Right]) instructions steps

    let startNodes =
        nodeMap.Values
        |> Seq.filter (fun k -> k.Name.[2] = startChar)
        |> Seq.toArray

    startNodes
    |> Array.Parallel.map (fun n -> traverse' n instructions 0UL)

// Because each node will end up at the same node, we can just find the LCM of the steps
let lcm nums =
    let rec gcd a b =
        if b = 0UL then a
        else gcd b (a % b)
    nums
    |> Array.fold (fun a b -> (a * b) / (gcd a b)) 1UL

let part2 filename =
    filename
    |> parse
    |> traverse2 ('A', 'Z')
    |> lcm