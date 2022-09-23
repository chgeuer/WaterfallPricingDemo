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

[<Test>]
let CreateMeterWithIncludedQuantities () =
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

    let expected = 
        { Total = 0UL; Consumption = Map.empty 
          Model = [
            FreeIncluded               (GigaByte 100UL)
            Range {   LowerIncluding =     100UL; UpperExcluding =  10_100UL; DimensionId = "Next 10TB / Month";  }
            Range {   LowerIncluding =  10_100UL; UpperExcluding =  50_100UL; DimensionId = "Next 40TB / Month" };
            Range {   LowerIncluding =  50_100UL; UpperExcluding = 150_100UL; DimensionId = "Next 100TB / Month"; };
            Range {   LowerIncluding = 150_100UL; UpperExcluding = 500_100UL; DimensionId = "Next 350TB / Month";  };
            Overage { LowerIncluding = 500_100UL;                             DimensionId = "Overage over 500TB" }
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
    |> assertConsumption (Map.empty |> Map.add "Next 10TB / Month"  (GigaByte 1UL))
    |> consume (GigaByte 1UL)
    |> assertTotal 102UL
    |> assertConsumption (Map.empty |> Map.add "Next 10TB / Month"  (GigaByte 2UL))
    |> ignore

[<Test>]
let CreateMeterWithOutIncludedQuantities () =
    let meter =
        [
            { Begin = GigaByte   0UL; Name = "First 100GB / Month" }
            { Begin = GigaByte 100UL; Name = "Next 10TB / Month" }
            { Begin = TeraByte  10UL; Name = "Next 40TB / Month" }
            { Begin = TeraByte  40UL; Name = "Next 100TB / Month" }
            { Begin = TeraByte 100UL; Name = "Next 350TB / Month" }
            { Begin = TeraByte 350UL; Name = "Overage over 500TB" }
        ]
        |> create

    let expected = 
        { Total = 0UL; Consumption = Map.empty 
          Model = [
            FreeIncluded                     0UL
            Range {   LowerIncluding =       0UL; UpperExcluding =     100UL; DimensionId = "First 100GB / Month";  }
            Range {   LowerIncluding =     100UL; UpperExcluding =  10_100UL; DimensionId = "Next 10TB / Month";  }
            Range {   LowerIncluding =  10_100UL; UpperExcluding =  50_100UL; DimensionId = "Next 40TB / Month" };
            Range {   LowerIncluding =  50_100UL; UpperExcluding = 150_100UL; DimensionId = "Next 100TB / Month"; };
            Range {   LowerIncluding = 150_100UL; UpperExcluding = 500_100UL; DimensionId = "Next 350TB / Month";  };
            Overage { LowerIncluding = 500_100UL;                             DimensionId = "Overage over 500TB" }
          ] }

    Assert.AreEqual(expected, meter)
