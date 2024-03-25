open FSharp.Data
open FSharp.Data.CsvExtensions
open System
open System.Text.RegularExpressions

type Transaction =
    | Purchase of
        {| Symbol: string
           Shares: float
           Price: float
           Date: DateOnly |}
    | Sell of
        {| Symbol: string
           Shares: float
           Price: float
           Date: DateOnly |}
    | Dividend of
        {| Symbol: string
           Amount: float
           Date: DateOnly |}
    | Interest of {| Amount: float; Date: DateOnly |}

type Lot =
    { Shares: float
      Price: float
      Purchased: DateOnly }

type Gain =
    { Shares: float
      Amount: float
      Realized: DateOnly }

let readTransactions (rows: seq<CsvRow>) =
    let (|IsPurchase|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*YOU BOUGHT") then
            Some(
                Purchase
                    {| Symbol = row?Symbol.Trim()
                       Shares = row?Quantity.AsFloat()
                       Price = row?Price.AsFloat()
                       Date = row.["Run Date"] |> DateOnly.Parse |}
            )
        else
            None

    let (|IsSell|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*YOU SOLD") then
            Some(
                Sell
                    {| Symbol = row?Symbol.Trim()
                       Shares = row?Quantity.AsFloat() |> Math.Abs
                       Price = row?Price.AsFloat()
                       Date = row.["Run Date"] |> DateOnly.Parse |}
            )
        else
            None

    let (|IsDividend|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*DIVIDEND RECEIVED") then
            Some(
                Dividend
                    {| Symbol = row?Symbol.Trim()
                       Amount = row?Amount.AsFloat()
                       Date = row.["Run Date"] |> DateOnly.Parse |}
            )
        else
            None

    let (|IsInterest|_|) (row: CsvRow) =
        if Regex.IsMatch(row?Action, @"^\s*INTEREST EARNED") then
            Some(
                Interest
                    {| Amount = row?Amount.AsFloat()
                       Date = row.["Run Date"] |> DateOnly.Parse |}
            )
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
    | Purchase p -> p.Date
    | Sell s -> s.Date
    | Dividend d -> d.Date
    | Interest i -> i.Date

let datesWithin (days: int) (a: DateOnly) (b: DateOnly) =
    Math.Abs(a.DayNumber - b.DayNumber) <= days

let capitalGainsFor (transactions: seq<Transaction>) (year: int) =
    let folder (state: Map<string, list<Lot> * list<Gain>>) =
        function
        | Purchase p ->
            let (ourLots, ourGains) =
                match Map.tryFind p.Symbol state with
                | Some(s, g) -> s, g
                | None -> [], []

            // Look back through realized losses for wash sales in FIFO order.
            let gainFolder ((sharesLeft, disallowed, newGains): float * float * list<Gain>) (gain: Gain) =
                if gain.Amount < 0 && datesWithin 30 gain.Realized p.Date then
                    if gain.Shares <= sharesLeft then
                        (sharesLeft - gain.Shares, disallowed + gain.Amount, newGains)
                    else
                        let washPct = sharesLeft / gain.Shares

                        (0.,
                         disallowed + -gain.Amount * washPct,
                         newGains
                         @ [ { Shares = gain.Shares - sharesLeft
                               Amount = gain.Amount * (1. - washPct)
                               Realized = gain.Realized } ])
                else
                    (sharesLeft, disallowed, newGains @ [ gain ])

            let _, disallowed, newGains = Seq.fold gainFolder (p.Shares, 0., []) ourGains

            Map.add
                p.Symbol
                ((ourLots
                  @ [ { Shares = p.Shares
                        Price = p.Price + disallowed / p.Shares
                        Purchased = p.Date } ]),
                 newGains)
                state
        | Sell s ->
            let (ourLots, ourGains) =
                match Map.tryFind s.Symbol state with
                | Some(s, g) -> s, g
                | None -> [], []

            // Sell lots in FIFO order.
            let lotFolder ((sharesLeft, gain, newLots): float * float * list<Lot>) (lot: Lot) =
                if lot.Shares > sharesLeft then
                    (0.,
                     gain + sharesLeft * (s.Price - lot.Price),
                     newLots
                     @ [ { Shares = lot.Shares - sharesLeft
                           Price = lot.Price
                           Purchased = lot.Purchased } ])
                else
                    (sharesLeft - lot.Shares, gain + lot.Shares * (s.Price - lot.Price), newLots)

            let sharesLeft, gain, newLots = Seq.fold lotFolder (s.Shares, 0., []) ourLots

            if sharesLeft > 0.0001 then
                eprintfn "Failed to find basis for %f shares of %s sold on %s." sharesLeft s.Symbol (s.Date.ToString())

            Map.add
                s.Symbol
                (newLots,
                 ourGains
                 @ [ { Shares = s.Shares
                       Amount = gain
                       Realized = s.Date } ])
                state
        | _ -> state

    transactions
    |> Seq.fold folder (Map.empty<string, list<Lot> * list<Gain>>)
    |> (fun state -> state.Values)
    |> Seq.map (fun (_lots, gains) -> gains)
    |> Seq.concat
    |> Seq.filter (fun gain ->
        let date = gain.Realized
        date >= DateOnly(year, 1, 1) && date <= DateOnly(year, 12, 31))
    |> Seq.sumBy (fun gain -> gain.Amount)

let filterFor (transactions: seq<Transaction>) (year: int) =
    Seq.filter
        (fun tx ->
            let date = getDate tx
            date >= DateOnly(year, 1, 1) && date <= DateOnly(year, 12, 31))
        transactions

let interestFor (transactions: seq<Transaction>) (year: int) =
    filterFor transactions year
    |> Seq.choose (function
        | Interest i -> Some i.Amount
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
        | Dividend d -> Some(d.Symbol, d.Amount)
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

    let gains = capitalGainsFor orderedTransactions year
    printfn "Gains Realized:\t\t%s" (cashf gains)

    let dividends = dividendsFor orderedTransactions year
    printfn "\nDividends Received:"
    Map.iter (fun symbol amount -> printfn "%s:\t\t\t%s" symbol (cashf amount)) dividends

    let interest = interestFor orderedTransactions year
    printfn "\nInterest Earned:\t%s" (cashf interest)

    0
