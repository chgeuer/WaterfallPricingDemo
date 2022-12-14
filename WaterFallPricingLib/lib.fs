namespace Waterfall

open System

type Quantity = uint64
type DimensionId = string

type WaterfallDescriptionItem = 
    { Begin: Quantity
      Name: string }

type Range =
    { DimensionId: DimensionId 
      LowerIncluding: Quantity
      UpperExcluding: Quantity }

type Overage =
    { DimensionId: DimensionId
      LowerIncluding: Quantity }

type WaterfallModelRow =  
  | FreeIncluded of Quantity // For included quantities, no reporting to the metering API
  | Range of Range
  | Overage of Overage

type WaterfallDescription =
  WaterfallDescriptionItem list

type WaterfallModel = 
  WaterfallModelRow list

module WaterfallModel =
  let expand (model: WaterfallDescription) : WaterfallModel =
    let len = model |> List.length
    let expanded =
      [0 .. len - 1]
      |> List.map (fun i -> 
        { Begin = 
            model
            |> List.take (i + 1)
            |> List.sumBy (fun x -> x.Begin)
          Name = 
            model
            |> List.skip(i)
            |> List.head
            |> fun x -> x.Name })

    let included =
      expanded
      |> List.head
      |> fun x -> x.Begin
      |> FreeIncluded

    let ranges =
      expanded |> List.skip 1
      |> List.zip (expanded |> List.take (len - 1))
      |> List.map (fun (l, r) -> { DimensionId = l.Name; LowerIncluding = l.Begin; UpperExcluding = r.Begin })
      |> List.map Range

    let overage =
      expanded
      |> List.last
      |> fun i -> { DimensionId = i.Name; LowerIncluding = i.Begin }
      |> Overage
       
    match included with
    | FreeIncluded x when x > 0UL -> included :: (ranges @ [overage])
    | _ -> (ranges @ [overage])
  
  let display : (WaterfallModelRow -> DimensionId * string) = function
    | FreeIncluded x -> ("Free", $"{x}")
    | Range x -> (x.DimensionId, $"{x.LowerIncluding}--{x.UpperExcluding-1UL}")
    | Overage x -> (x.DimensionId, $"{x.LowerIncluding}--Infinity")

/// These must be reported
type ConsumptionReport = 
  { DimensionId: DimensionId
    Quantity: Quantity }

type WaterfallMeter =
  { Model: WaterfallModel 
    Total: Quantity
    Consumption: Map<DimensionId, Quantity> }

type SubtractionAggregation =
  { CurrentTotal: Quantity 
    AmountToBeDeducted: Quantity 
    Consumption: Map<DimensionId, Quantity> }

module WaterfallMeter =
  open WaterfallModel

  let create description = 
     { Model = description |> expand
       Total = 0UL
       Consumption = Map.empty }

  let setTotal newTotal meter = 
    { meter with Total = newTotal }
  
  /// Identify the ranges into which the amount might fit.  
  let findRange (amount: Quantity) (model: WaterfallModel) : WaterfallModelRow list =
    /// Determine if the current total matches the given row.
    let isNotInRow (currentTotal: Quantity) (row: WaterfallModelRow) : bool =
      match row with
      | FreeIncluded x -> currentTotal < x
      | Range { LowerIncluding = lower; UpperExcluding = upper } -> lower <= currentTotal && currentTotal < upper
      | Overage { LowerIncluding = lower } -> lower <= currentTotal
      |> not

    model
    |> List.skipWhile (isNotInRow amount)

  let subtract (agg: SubtractionAggregation) (row: WaterfallModelRow) : SubtractionAggregation =
    let add (v: Quantity) = function
        | None -> Some v
        | Some e -> Some (v + e)

    let augment ct a c agg = 
      match c with
      | Some c when c.Quantity > 0UL -> { CurrentTotal = ct; AmountToBeDeducted = a; Consumption = agg.Consumption |> Map.change c.DimensionId (add c.Quantity) }
      | _ -> { CurrentTotal = ct; AmountToBeDeducted = a; Consumption = agg.Consumption } // Do not add empty consumption records

    let newTotal = agg.CurrentTotal + agg.AmountToBeDeducted
    match row with
    | FreeIncluded x when newTotal < x -> agg |> augment newTotal 0UL None
    | FreeIncluded x -> agg |> augment x (newTotal - x) None
    | Range { UpperExcluding = upper; DimensionId = did } when newTotal < upper -> agg |> augment newTotal 0UL (Some { DimensionId = did; Quantity = agg.AmountToBeDeducted})
    | Range { UpperExcluding = upper; DimensionId = did } -> agg |> augment upper (newTotal - upper) (Some { DimensionId = did; Quantity = upper - agg.CurrentTotal})
    | Overage { DimensionId = dim } -> agg |> augment newTotal 0UL (Some { DimensionId = dim; Quantity = agg.AmountToBeDeducted })

  let consume (amount: Quantity) (meter: WaterfallMeter) =
    findRange meter.Total meter.Model
    |> List.fold subtract { CurrentTotal = meter.Total; AmountToBeDeducted = amount; Consumption = meter.Consumption } 
    |> fun agg ->
        { meter with 
            Total = agg.CurrentTotal 
            Consumption = agg.Consumption }
