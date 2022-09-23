module TestProject1

open NUnit.Framework
open Waterfall
open WaterfallMeter

[<SetUp>]
let Setup () = ()

let GigaByte x = x
let TeraByte x = 1000UL * GigaByte x

let assertTotal expected meter =
    Assert.AreEqual(expected, meter.Total)
    meter

let assertConsumption expected (meter: WaterfallMeter) =
    Assert.AreEqual(expected, meter.Consumption)
    meter

let tier0_0_100 = "First 100GB / Month"
let tier1_100_10100 = "Next 10TB / Month"
let tier2_10100_50100 = "Next 40TB / Month"
let tier3_50100_150100 = "Next 100TB / Month"
let tier4_150100_500099= "Next 350TB / Month"
let tier5_500100_and_more = "Overage over 500TB"

[<Test>]
let CreateMeterWithIncludedQuantities () =
    let meter =
        [
            // { Begin = GigaByte 0UL ; Name = "First 100GB / Month" }
            { Begin = GigaByte 100UL; Name = tier1_100_10100 }
            { Begin = TeraByte  10UL; Name = tier2_10100_50100 }
            { Begin = TeraByte  40UL; Name = tier3_50100_150100 }
            { Begin = TeraByte 100UL; Name = tier4_150100_500099 }
            { Begin = TeraByte 350UL; Name = tier5_500100_and_more }
        ]
        |> create

    let expected = 
        { Total = 0UL; Consumption = Map.empty 
          Model = [
            FreeIncluded               (GigaByte     100UL)
            Range {   LowerIncluding = (GigaByte     100UL); UpperExcluding = (GigaByte  10_100UL); DimensionId = tier1_100_10100  }
            Range {   LowerIncluding = (GigaByte  10_100UL); UpperExcluding = (GigaByte  50_100UL); DimensionId = tier2_10100_50100 }
            Range {   LowerIncluding = (GigaByte  50_100UL); UpperExcluding = (GigaByte 150_100UL); DimensionId = tier3_50100_150100 }
            Range {   LowerIncluding = (GigaByte 150_100UL); UpperExcluding = (GigaByte 500_100UL); DimensionId = tier4_150100_500099 }
            Overage { LowerIncluding = (GigaByte 500_100UL);                                        DimensionId = tier5_500100_and_more }
          ] }

    Assert.AreEqual(expected, meter)

    meter
    |> consume (GigaByte 50UL)
    |> assertTotal 50UL
    |> assertConsumption Map.empty
    |> consume (GigaByte 49UL)
    |> assertTotal 99UL
    |> assertConsumption Map.empty
    |> consume (GigaByte 2UL)
    |> assertTotal 101UL
    |> assertConsumption (Map.empty |> Map.add tier1_100_10100 (GigaByte 1UL))
    |> consume (GigaByte 1UL)
    |> assertTotal 102UL
    |> assertConsumption (Map.empty |> Map.add tier1_100_10100  (GigaByte 2UL))
    |> ignore

[<Test>]
let CreateMeterWithOutIncludedQuantities () =
    let meter =
        [
            { Begin = GigaByte   0UL; Name = tier0_0_100 }
            { Begin = GigaByte 100UL; Name = tier1_100_10100 }
            { Begin = TeraByte  10UL; Name = tier2_10100_50100 }
            { Begin = TeraByte  40UL; Name = tier3_50100_150100 }
            { Begin = TeraByte 100UL; Name = tier4_150100_500099 }
            { Begin = TeraByte 350UL; Name = tier5_500100_and_more }
        ]
        |> create

    let expected = 
        { Total = 0UL; Consumption = Map.empty 
          Model = [
            FreeIncluded               (GigaByte       0UL)
            Range {   LowerIncluding = (GigaByte       0UL); UpperExcluding = (GigaByte     100UL); DimensionId = tier0_0_100 }
            Range {   LowerIncluding = (GigaByte     100UL); UpperExcluding = (GigaByte  10_100UL); DimensionId = tier1_100_10100 }
            Range {   LowerIncluding = (GigaByte  10_100UL); UpperExcluding = (GigaByte  50_100UL); DimensionId = tier2_10100_50100 }
            Range {   LowerIncluding = (GigaByte  50_100UL); UpperExcluding = (GigaByte 150_100UL); DimensionId = tier3_50100_150100 }
            Range {   LowerIncluding = (GigaByte 150_100UL); UpperExcluding = (GigaByte 500_100UL); DimensionId = tier4_150100_500099 }
            Overage { LowerIncluding = (GigaByte 500_100UL);                                        DimensionId = tier5_500100_and_more }
          ] }

    Assert.AreEqual(expected, meter)
