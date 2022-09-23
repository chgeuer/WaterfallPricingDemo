module TestProject1

open NUnit.Framework
open Waterfall
open WaterfallMeter

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    let GigaByte x = x
    let TeraByte x = 1000UL * GigaByte x
    let assertTotal (expected: Quantity) (meter: WaterfallMeter) : WaterfallMeter =
        NUnit.Framework.Assert.AreEqual(expected, meter.Total)
        meter

    let meter =
        [
            // { Begin = GigaByte 0UL ; Name = "First 100GB / Month" }
            { Begin = GigaByte 100UL; Name = "Next 10TB / Month" }
            { Begin = TeraByte  10UL; Name = "Next 40TB / Month" }
            { Begin = TeraByte  40UL; Name = "Next 100TB / Month" }
            { Begin = TeraByte 100UL; Name = "Next 350TB / Month" }
            { Begin = TeraByte 350UL; Name = "Overage over 500TB" }
        ]
        |> create
        |> setTotal (0UL)
        |> consume (GigaByte 100UL)
        |> assertTotal 100UL
        |> consume (GigaByte 1UL)
        |> assertTotal 101UL


    meter
    |> (fun x -> x.Consumption)
    |> Map.toList
    |> List.map (fun (did, q) -> $"{did}: {q}")
    |> List.iter (fun x -> printfn "%s" x)
