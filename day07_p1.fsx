open System
type Card =
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6
    | Seven = 7
    | Eight = 8
    | Nine = 9
    | Ten = 10
    | Jack = 11
    | Queen = 12
    | King = 13
    | Ace = 14

type HandType =
    | HighCard = 0
    | OnePair = 1
    | TwoPairs = 2
    | ThreeOfAKind = 3
    | FullHouse = 4
    | FourOfAKind = 5
    | FiveOfAKind = 6

[<CustomComparison; CustomEquality>]
type Hand =
    {
        Cards: Card list
        Bid: uint64
    }
    member this.Type =
        match this.Cards |> List.groupBy id |> List.map (fun (k, v) -> v |> List.length) |> List.sortDescending with
        | 5 :: _        -> HandType.FiveOfAKind
        | 4 :: _        -> HandType.FourOfAKind
        | 3 :: 2 :: _   -> HandType.FullHouse
        | 3 :: _        -> HandType.ThreeOfAKind
        | 2 :: 2 :: _   -> HandType.TwoPairs
        | 2 :: _        -> HandType.OnePair
        | _             -> HandType.HighCard

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Hand as p -> (this :> IComparable<_>).CompareTo p
            | _ -> -1

    interface IComparable<Hand> with
        member this.CompareTo other =
            let rec compareCards (left : Card list) (right : Card list) =
                match left, right with
                | [], [] -> 0
                | [], _ -> -1
                | _, [] -> 1
                | lhead::ltail, rhead::rtail ->
                    match lhead.CompareTo rhead with
                    | 0 -> compareCards ltail rtail
                    | x -> x
            match other.Type.CompareTo this.Type with
            | 0 -> compareCards other.Cards this.Cards
            | x -> x

    override this.Equals other =
        match other with
        | :? Hand as p -> (this :> IEquatable<_>).Equals p
        | _ -> false

    interface IEquatable<Hand> with
        member this.Equals other = other.Cards = this.Cards && other.Bid = this.Bid

    override this.GetHashCode () = HashCode.Combine(this.Cards, this.Bid)

let parseHand (input : string) =
    let tokens = input.Split(' ', 2, System.StringSplitOptions.RemoveEmptyEntries)
    let bid = uint64 tokens.[1]
    let cards =
        tokens.[0]
        |> Seq.map (fun c ->
            match c with
            | '2' -> Card.Two
            | '3' -> Card.Three
            | '4' -> Card.Four
            | '5' -> Card.Five
            | '6' -> Card.Six
            | '7' -> Card.Seven
            | '8' -> Card.Eight
            | '9' -> Card.Nine
            | 'T' -> Card.Ten
            | 'J' -> Card.Jack
            | 'Q' -> Card.Queen
            | 'K' -> Card.King
            | 'A' -> Card.Ace 
            | _ -> failwithf "Invalid card %A" c)
        |> Seq.toList
    { Cards = cards; Bid = bid }

let parse (filename : string) =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.map parseHand

let part1 filename =
    filename
    |> parse
    |> Array.sortDescending
    |> Array.indexed
    |> Array.sumBy (fun (i, hand) -> hand.Bid * (uint64 i + 1UL))