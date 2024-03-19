open FSharp.Data
open FSharp.Data.CsvExtensions
open System
open System.Text.RegularExpressions

type Transaction =
    | Purchase of symbol: string * shares: float * price: float * date: DateOnly
    | Sell of symbol: string * shares: float * price: float * date: DateOnly
    | Dividend of symbol: string * amount: float * date: DateOnly
    | Interest of amount: float * date: DateOnly

let readTransactions (rows: seq<CsvRow>) =
    let (|IsPurchase|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*YOU BOUGHT") then
            Some(
                Purchase(
                    symbol = row?Symbol.Trim(),
                    shares = row?Quantity.AsFloat(),
                    price = row?Price.AsFloat(),
                    date = (row.["Run Date"] |> DateOnly.Parse)
                )
            )
        else
            None

    let (|IsSell|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*YOU SOLD") then
            Some(
                Sell(
                    symbol = row?Symbol.Trim(),
                    shares = (row?Quantity.AsFloat() |> Math.Abs),
                    price = row?Price.AsFloat(),
                    date = (row.["Run Date"] |> DateOnly.Parse)
                )
            )
        else
            None

    let (|IsDividend|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*DIVIDEND RECEIVED") then
            Some(
                Dividend(
                    symbol = row?Symbol.Trim(),
                    amount = row?Amount.AsFloat(),
                    date = (row.["Run Date"] |> DateOnly.Parse)
                )
            )
        else
            None

    let (|IsInterest|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*INTEREST EARNED") then
            Some(Interest(amount = row?Amount.AsFloat(), date = (row.["Run Date"] |> DateOnly.Parse)))
        else
            None

    Seq.choose
        (function
        | IsPurchase t -> Some t
        | IsSell t -> Some t
        | IsDividend t -> Some t
        | IsInterest t -> Some t
        | _ -> None)
        rows

let getDate =
    function
    | Purchase(_, _, _, d) -> d
    | Sell(_, _, _, d) -> d
    | Dividend(_, _, d) -> d
    | Interest(_, d) -> d

let filterFor (transactions: seq<Transaction>) (year: int) =
    Seq.filter
        (fun tx ->
            let date = getDate tx
            date >= DateOnly(year, 1, 1) && date <= DateOnly(year, 12, 31))
        transactions

let interestFor (transactions: seq<Transaction>) (year: int) =
    filterFor transactions year
    |> Seq.choose (function
        | Interest(amount, _) -> Some amount
        | _ -> None)
    |> Seq.sum

let dividendsFor (transactions: seq<Transaction>) (year: int) =
    let folder (state: Map<string, float>) ((symbol, amount): string * float) =
        let sum =
            match Map.tryFind symbol state with
            | Some s -> s
            | None -> 0

        Map.add symbol (sum + amount) state

    filterFor transactions year
    |> Seq.choose (function
        | Dividend(symbol, amount, _) -> Some(symbol, amount)
        | _ -> None)
    |> Seq.fold folder Map.empty<string, float>

let cashf = sprintf "$ %.2f"

[<EntryPoint>]
let main args =
    let csvPath, year =
        match args with
        | [| a0; a1 |] -> a0, int a1
        | _ -> failwith "Expected arguments: <csv file> <year>"

    let csvRows = CsvFile.Load(csvPath, ignoreErrors = true).Rows

    let orderedTransactions =
        readTransactions csvRows |> Seq.toList |> List.sortBy getDate

    let dividends = dividendsFor orderedTransactions year
    printfn "Dividends Received:"
    Map.iter (fun symbol amount -> printfn "%s:\t%s" symbol (cashf amount)) dividends

    let interest = interestFor orderedTransactions year
    printfn "\nInterest Earned:\t%s" (cashf interest)

    0
