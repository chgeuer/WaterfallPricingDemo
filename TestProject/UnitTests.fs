module TestProject1

open NUnit.Framework
open Waterfall
open WaterfallMeter

[<SetUp>]
let Setup () = ()

let GigaByte x = x
let TeraByte x = 1000UL * GigaByte x // yeah, I know, 1024, but this helps readability

let assertTotal expected meter =
    Assert.AreEqual(expected, meter.Total)
    meter

let assertConsumption expected (meter: WaterfallMeter) =
    Assert.AreEqual(expected, meter.Consumption)
    meter

let tier0_0_99 = "First 100GB / Month"
let tier1_100_10099 = "Next 10TB / Month"
let tier2_10100_50099 = "Next 40TB / Month"
let tier3_50100_150099 = "Next 100TB / Month"
let tier4_150100_500099= "Next 350TB / Month"
let tier5_500100_and_more = "Overage over 500TB"

[<Test>]
let CreateMeterWithIncludedQuantities () =
    let meter =
        [
            { Begin = GigaByte 100UL; Name = tier1_100_10099 }
            { Begin = TeraByte  10UL; Name = tier2_10100_50099 }
            { Begin = TeraByte  40UL; Name = tier3_50100_150099 }
            { Begin = TeraByte 100UL; Name = tier4_150100_500099 }
            { Begin = TeraByte 350UL; Name = tier5_500100_and_more }
        ]
        |> create

    let expected = 
        { Total = 0UL; Consumption = Map.empty 
          Model = [
            FreeIncluded               (GigaByte     100UL)
            Range {   LowerIncluding = (GigaByte     100UL); UpperExcluding = (GigaByte  10_100UL); DimensionId = tier1_100_10099 }
            Range {   LowerIncluding = (GigaByte  10_100UL); UpperExcluding = (GigaByte  50_100UL); DimensionId = tier2_10100_50099 }
            Range {   LowerIncluding = (GigaByte  50_100UL); UpperExcluding = (GigaByte 150_100UL); DimensionId = tier3_50100_150099 }
            Range {   LowerIncluding = (GigaByte 150_100UL); UpperExcluding = (GigaByte 500_100UL); DimensionId = tier4_150100_500099 }
            Overage { LowerIncluding = (GigaByte 500_100UL);                                        DimensionId = tier5_500100_and_more }
          ] }

    Assert.AreEqual(expected, meter)

    let submitDataToMeteringAndEmptyConsumption (x: WaterfallMeter) = { x with Consumption = Map.empty }

    meter
    |> consume (GigaByte     50UL) |> assertTotal      50UL |> assertConsumption Map.empty
    |> consume (GigaByte     49UL) |> assertTotal      99UL |> assertConsumption Map.empty
    |> consume (GigaByte      2UL) |> assertTotal     101UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte      1UL))])
    |> consume (GigaByte      1UL) |> assertTotal     102UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte      2UL))])
    |> consume (GigaByte  9_997UL) |> assertTotal  10_099UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte  9_999UL))])
    |> consume (GigaByte      1UL) |> assertTotal  10_100UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL))])
    |> consume (GigaByte      1UL) |> assertTotal  10_101UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL)); (tier2_10100_50099, (GigaByte 1UL))])
    |> consume (GigaByte      2UL) |> assertTotal  10_103UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL)); (tier2_10100_50099, (GigaByte 3UL))])
    |> consume (GigaByte 39_996UL) |> assertTotal  50_099UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL)); (tier2_10100_50099, (GigaByte 39_999UL))])
    |> consume (GigaByte      1UL) |> assertTotal  50_100UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL)); (tier2_10100_50099, (GigaByte 40_000UL))])
    |> consume (GigaByte      1UL) |> assertTotal  50_101UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL)); (tier2_10100_50099, (GigaByte 40_000UL)); (tier3_50100_150099, (GigaByte 1UL))])
    |> consume (GigaByte 99_998UL) |> assertTotal 150_099UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL)); (tier2_10100_50099, (GigaByte 40_000UL)); (tier3_50100_150099, (GigaByte 99_999UL))])
    |> consume (GigaByte      2UL) |> assertTotal 150_101UL |> assertConsumption (Map.ofSeq [(tier1_100_10099, (GigaByte 10_000UL)); (tier2_10100_50099, (GigaByte 40_000UL)); (tier3_50100_150099, (GigaByte 100_000UL)); (tier4_150100_500099, (GigaByte 1UL))])
    |> submitDataToMeteringAndEmptyConsumption // simulate we submitted the data. 1 unit from tier4 is submitted here already, that's why the remaining quantity for tier4 is 349_999UL
    |> consume (GigaByte      2UL) |> assertTotal 150_103UL |> assertConsumption (Map.ofSeq [(tier4_150100_500099, (GigaByte 2UL))])
    |> consume (GigaByte 350_000UL) |> assertTotal 500_103UL |> assertConsumption (Map.ofSeq [(tier4_150100_500099, (GigaByte 349_999UL)); (tier5_500100_and_more, (GigaByte 3UL))])
    |> ignore

[<Test>]
let CreateMeterWithOutIncludedQuantities () =
    let meter =
        [
            { Begin = GigaByte   0UL; Name = tier0_0_99 }
            { Begin = GigaByte 100UL; Name = tier1_100_10099 }
            { Begin = TeraByte  10UL; Name = tier2_10100_50099 }
            { Begin = TeraByte  40UL; Name = tier3_50100_150099 }
            { Begin = TeraByte 100UL; Name = tier4_150100_500099 }
            { Begin = TeraByte 350UL; Name = tier5_500100_and_more }
        ]
        |> create

    let expected = 
        { Total = 0UL; Consumption = Map.empty 
          Model = [
            Range {   LowerIncluding = (GigaByte       0UL); UpperExcluding = (GigaByte     100UL); DimensionId = tier0_0_99 }
            Range {   LowerIncluding = (GigaByte     100UL); UpperExcluding = (GigaByte  10_100UL); DimensionId = tier1_100_10099 }
            Range {   LowerIncluding = (GigaByte  10_100UL); UpperExcluding = (GigaByte  50_100UL); DimensionId = tier2_10100_50099 }
            Range {   LowerIncluding = (GigaByte  50_100UL); UpperExcluding = (GigaByte 150_100UL); DimensionId = tier3_50100_150099 }
            Range {   LowerIncluding = (GigaByte 150_100UL); UpperExcluding = (GigaByte 500_100UL); DimensionId = tier4_150100_500099 }
            Overage { LowerIncluding = (GigaByte 500_100UL);                                        DimensionId = tier5_500100_and_more }
          ] }

    Assert.AreEqual(expected, meter)
