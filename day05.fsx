open System
open System.IO
open System.Text.RegularExpressions

type Almanac = {
    Seeds : uint64 list
    SeedToSoil : Map<uint64,uint64>
    SoilToFertilizer : Map<uint64,uint64>
    FertilizerToWater : Map<uint64,uint64>
    WaterToLight : Map<uint64,uint64>
    LightToTemperature : Map<uint64,uint64>
    TemperatureToHumidity : Map<uint64,uint64>
    HumidityToLocation : Map<uint64,uint64>
} with
    static member Empty = {
        Seeds = List.Empty
        SeedToSoil = Map.empty
        SoilToFertilizer = Map.empty
        FertilizerToWater = Map.empty
        WaterToLight = Map.empty
        LightToTemperature = Map.empty
        TemperatureToHumidity = Map.empty
        HumidityToLocation = Map.empty
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

let range src dst len =
    seq {dst..(dst+len)}
    |> Seq.zip (seq {src..(src+len)})

let map ranges =
    ranges
    |> Seq.concat
    |> Map.ofSeq

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
        | MapRange (s,d,len) -> (almanac, (src,dst), (range s d len)::ranges)
        | "" -> 
            let map = map ranges
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

let convert almanac =
    let getnum map key =
        match Map.tryFind key map with
        | Some value -> value
        | None -> key

    almanac.Seeds
    |> List.map (fun seed -> 
        let seed = seed
        let soil = getnum almanac.SeedToSoil seed
        let fertilizer = getnum almanac.SoilToFertilizer soil
        let water = getnum almanac.FertilizerToWater fertilizer
        let light = getnum almanac.WaterToLight water
        let temperature = getnum almanac.LightToTemperature light
        let humidity = getnum almanac.TemperatureToHumidity temperature
        let location = getnum almanac.HumidityToLocation humidity
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
    |> convert
    |> List.minBy (fun plant -> plant.Location)