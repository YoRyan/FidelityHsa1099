open FSharp.Data
open FSharp.Data.CsvExtensions
open System
open System.Text.RegularExpressions

type Transaction =
    | Purchase of symbol: string * shares: float * price: float * date: DateOnly
    | Sell of symbol: string * shares: float * price: float * date: DateOnly
    | Dividend of symbol: string * amount: float * date: DateOnly

let readTransactions (rows: seq<CsvRow>) =
    let (|IsPurchase|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*YOU BOUGHT") then
            Some(
                Purchase(
                    symbol = (row?Symbol).Trim(),
                    shares = (row?Quantity).AsFloat(),
                    price = (row.["Price ($)"]).AsFloat(),
                    date = DateOnly.Parse(row.["Run Date"])
                )
            )
        else
            None

    let (|IsSell|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*YOU SOLD") then
            Some(
                Sell(
                    symbol = (row?Symbol).Trim(),
                    shares = ((row?Quantity).AsFloat() |> Math.Abs),
                    price = (row.["Price ($)"]).AsFloat(),
                    date = (row.["Run Date"] |> DateOnly.Parse)
                )
            )
        else
            None

    let (|IsDividend|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*DIVIDEND RECEIVED") then
            Some(
                Dividend(
                    symbol = (row?Symbol).Trim(),
                    amount = row.["Amount ($)"].AsFloat(),
                    date = (row.["Run Date"] |> DateOnly.Parse)
                )
            )
        else
            None

    Seq.choose
        (function
        | IsPurchase t -> Some t
        | IsSell t -> Some t
        | IsDividend t -> Some t
        | _ -> None)
        rows

[<EntryPoint>]
let main args =
    let csvPath =
        match args with
        | [| a0 |] -> a0
        | _ -> failwith "Expected arguments: <csv file>"

    let csvRows = CsvFile.Load(csvPath, ignoreErrors = true).Rows

    let orderedTransactions =
        readTransactions csvRows
        |> Seq.toList
        |> List.sortBy (function
            | Purchase(_, _, _, date) -> date
            | Sell(_, _, _, date) -> date
            | Dividend(_, _, date) -> date)

    for tx in orderedTransactions do
        printfn "%s" (tx.ToString())

    0
