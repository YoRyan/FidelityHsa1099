module FidelityHsa1099.Tests

open NUnit.Framework
open Program
open System

[<SetUp>]
let Setup () = ()

[<Test>]
let InterestSum () =
    let transactions =
        [ Interest
              {| Amount = 100
                 Date = DateOnly(2024, 1, 1) |}
          Interest
              {| Amount = 150
                 Date = DateOnly(2024, 3, 1) |}
          Interest
              {| Amount = 50
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(300, interestFor transactions 2024)

[<Test>]
let InterestIgnoresOtherTransactions () =
    let transactions =
        [ Interest
              {| Amount = 100
                 Date = DateOnly(2024, 1, 1) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 50
                 Date = DateOnly(2024, 2, 1) |}
          Interest
              {| Amount = 50
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(150, interestFor transactions 2024)

[<Test>]
let InterestIgnoresOtherYears () =
    let transactions =
        [ Interest
              {| Amount = 75
                 Date = DateOnly(2023, 12, 31) |}
          Interest
              {| Amount = 100
                 Date = DateOnly(2024, 1, 1) |}
          Interest
              {| Amount = 50
                 Date = DateOnly(2024, 4, 20) |}
          Interest
              {| Amount = 35
                 Date = DateOnly(2025, 1, 15) |} ]

    Assert.AreEqual(150, interestFor transactions 2024)

[<Test>]
let DividendsSumOneSymbol () =
    let transactions =
        [ Dividend
              {| Symbol = "SPY"
                 Amount = 100
                 Date = DateOnly(2024, 1, 1) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 150
                 Date = DateOnly(2024, 3, 1) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 50
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(Map<string, float> [ ("SPY", 300) ], dividendsFor transactions 2024)

[<Test>]
let DividendsSumMultipleSymbols () =
    let transactions =
        [ Dividend
              {| Symbol = "SPY"
                 Amount = 100
                 Date = DateOnly(2024, 1, 1) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 150
                 Date = DateOnly(2024, 3, 1) |}
          Dividend
              {| Symbol = "VOO"
                 Amount = 50
                 Date = DateOnly(2024, 4, 20) |}
          Dividend
              {| Symbol = "VOO"
                 Amount = 30
                 Date = DateOnly(2024, 1, 6) |} ]

    Assert.AreEqual(Map<string, float> [ ("SPY", 250); ("VOO", 80) ], dividendsFor transactions 2024)

[<Test>]
let DividendsIgnoresOtherTransactions () =
    let transactions =
        [ Dividend
              {| Symbol = "SPY"
                 Amount = 100
                 Date = DateOnly(2024, 1, 1) |}
          Interest
              {| Amount = 25
                 Date = DateOnly(2024, 2, 1) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 50
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(Map<string, float> [ ("SPY", 150) ], dividendsFor transactions 2024)

[<Test>]
let DividendsIgnoresOtherYears () =
    let transactions =
        [ Dividend
              {| Symbol = "SPY"
                 Amount = 100
                 Date = DateOnly(2024, 1, 1) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 150
                 Date = DateOnly(2024, 3, 1) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 50
                 Date = DateOnly(2024, 4, 20) |}
          Dividend
              {| Symbol = "SPY"
                 Amount = 35
                 Date = DateOnly(2025, 1, 15) |} ]

    Assert.AreEqual(Map<string, float> [ ("SPY", 300) ], dividendsFor transactions 2024)

[<Test>]
let CapitalGainsBasicSell () =
    let transactions =
        [ Purchase
              {| Symbol = "SPY"
                 Shares = 24
                 Price = 100
                 Date = DateOnly(2024, 1, 1) |}
          Sell
              {| Symbol = "SPY"
                 Shares = 24
                 Price = 110
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(240, capitalGainsFor transactions 2024)

[<Test>]
let CapitalGainsBasicPartialSale () =
    let transactions =
        [ Purchase
              {| Symbol = "SPY"
                 Shares = 24
                 Price = 100
                 Date = DateOnly(2024, 1, 1) |}
          Sell
              {| Symbol = "SPY"
                 Shares = 12
                 Price = 110
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(120, capitalGainsFor transactions 2024)

[<Test>]
let CapitalGainsMultipleSell () =
    let transactions =
        [ Purchase
              {| Symbol = "SPY"
                 Shares = 100
                 Price = 5
                 Date = DateOnly(2024, 1, 1) |}
          Purchase
              {| Symbol = "VOO"
                 Shares = 100
                 Price = 10
                 Date = DateOnly(2024, 2, 1) |}
          Sell
              {| Symbol = "SPY"
                 Shares = 100
                 Price = 10
                 Date = DateOnly(2024, 4, 20) |}
          Sell
              {| Symbol = "VOO"
                 Shares = 100
                 Price = 15
                 Date = DateOnly(2024, 6, 10) |} ]

    Assert.AreEqual(1000, capitalGainsFor transactions 2024)

[<Test>]
let CapitalGainsMultipleLots () =
    let transactions =
        [ Purchase
              {| Symbol = "SPY"
                 Shares = 100
                 Price = 5
                 Date = DateOnly(2024, 1, 1) |}
          Purchase
              {| Symbol = "SPY"
                 Shares = 100
                 Price = 10
                 Date = DateOnly(2024, 2, 1) |}
          Sell
              {| Symbol = "SPY"
                 Shares = 200
                 Price = 20
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(1500 + 1000, capitalGainsFor transactions 2024)

[<Test>]
let CapitalGainsFifoLots () =
    let transactions =
        [ Purchase
              {| Symbol = "SPY"
                 Shares = 100
                 Price = 5
                 Date = DateOnly(2024, 1, 1) |}
          Purchase
              {| Symbol = "SPY"
                 Shares = 100
                 Price = 10
                 Date = DateOnly(2024, 2, 1) |}
          Sell
              {| Symbol = "SPY"
                 Shares = 150
                 Price = 10
                 Date = DateOnly(2024, 4, 20) |} ]

    Assert.AreEqual(500 + 0, capitalGainsFor transactions 2024)

[<Test>]
let CapitalGainsIgnoresOtherYears () =
    let transactions =
        [ Purchase
              {| Symbol = "SPY"
                 Shares = 100
                 Price = 10
                 Date = DateOnly(2024, 1, 1) |}
          Sell
              {| Symbol = "SPY"
                 Shares = 50
                 Price = 20
                 Date = DateOnly(2024, 12, 1) |}
          Sell
              {| Symbol = "SPY"
                 Shares = 50
                 Price = 30
                 Date = DateOnly(2025, 1, 1) |} ]

    Assert.AreEqual(500, capitalGainsFor transactions 2024)
