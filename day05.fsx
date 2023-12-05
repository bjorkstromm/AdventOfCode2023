open System
open System.IO
open System.Text.RegularExpressions

type Range =
    {
        Min : uint64
        Max : uint64
        Offset : uint64
    }   
    static member convert value this  =
        if value < this.Min || value > this.Max
        then None
        else Some (value - this.Min + this.Offset)
    static member create src dst len =
        {
            Min = src
            Max = src + len
            Offset = dst
        }

[<TailCall>]
let rec convert value ranges =
    match ranges with
    | head::tail -> 
        match head |> Range.convert value with
        | Some value -> value
        | None -> tail |> convert value
    | [] -> value

type Almanac = {
    Seeds : uint64 list
    SeedToSoil : Range list
    SoilToFertilizer : Range list
    FertilizerToWater : Range list
    WaterToLight : Range list
    LightToTemperature : Range list
    TemperatureToHumidity : Range list
    HumidityToLocation : Range list
} with
    static member Empty = {
        Seeds = List.Empty
        SeedToSoil = List.empty
        SoilToFertilizer = List.empty
        FertilizerToWater = List.empty
        WaterToLight = List.empty
        LightToTemperature = List.empty
        TemperatureToHumidity = List.empty
        HumidityToLocation = List.empty
    }

type Plant = {
    Seed : uint64
    Soil : uint64
    Fertilizer : uint64
    Water : uint64
    Light : uint64
    Temperature : uint64
    Humidity : uint64
    Location : uint64
}

let (|Integer|_|) (str: string) =
   let mutable value = 0UL
   if System.UInt64.TryParse(str, &value) then Some(value)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Split|) (separator: char) (str: string) =
    str.Split (separator, StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let (|Seeds|_|) (input : string) =
    match input with
    | ParseRegex @"seeds:\s+([0-9\s]+)" [Split ' ' seeds] -> seeds |> List.map uint64 |> Some
    | _ -> None

let (|MapHeader|_|) (input : string) =
    match input with
    | ParseRegex @"([a-z]+)-to-([a-z]+) map:" [src;dst] -> (src,dst) |> Some
    | _ -> None

let (|MapRange|_|) (input : string) =
    match input with
    | Split ' ' [Integer dst; Integer src; Integer len] -> (src,dst,len) |> Some
    | _ -> None

let parse filename =
    let folder (almanac, (src,dst), ranges) line =
        match line with
        | Seeds seeds -> ({ almanac with Seeds = seeds }, (src,dst), ranges)
        | MapHeader (src,dst) -> (almanac, (src,dst), ranges)
        | MapRange (s,d,len) -> (almanac, (src,dst), (Range.create s d len)::ranges)
        | "" -> 
            let map = ranges |> List.rev
            let almanac = match (src,dst) with
                          | ("seed","soil") -> { almanac with SeedToSoil = map }
                          | ("soil","fertilizer") -> { almanac with SoilToFertilizer = map }
                          | ("fertilizer","water") -> { almanac with FertilizerToWater = map }
                          | ("water","light") -> { almanac with WaterToLight = map }
                          | ("light","temperature") -> { almanac with LightToTemperature = map }
                          | ("temperature","humidity") -> { almanac with TemperatureToHumidity = map }
                          | ("humidity","location") -> { almanac with HumidityToLocation = map }
                          | _ -> almanac
            (almanac, ("",""), List.empty)
        | s -> failwithf "Unexpected input: %s" s
    
    let (almanac, _, _) =
        [|""|]
        |> Array.append (File.ReadAllLines filename)
        |> Array.fold folder (Almanac.Empty, ("",""), List.empty)
    almanac

let exec almanac =
    almanac.Seeds
    |> List.map (fun seed -> 
        let seed = seed
        let soil = almanac.SeedToSoil |> convert seed
        let fertilizer = almanac.SoilToFertilizer |> convert soil
        let water = almanac.FertilizerToWater |> convert fertilizer
        let light = almanac.WaterToLight |> convert water
        let temperature = almanac.LightToTemperature |> convert light
        let humidity = almanac.TemperatureToHumidity |> convert temperature
        let location = almanac.HumidityToLocation |> convert humidity
        { 
            Seed = seed
            Soil = soil
            Fertilizer = fertilizer
            Water = water
            Light = light
            Temperature = temperature
            Humidity = humidity
            Location = location
        })

let part1 filename =
    filename
    |> parse
    |> exec
    |> List.minBy (fun plant -> plant.Location)