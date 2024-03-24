module FidelityHsa1099.Tests

open NUnit.Framework
open Program
open System

[<SetUp>]
let Setup () = ()

[<Test>]
let InterestSum () =
    let transactions =
        [ Interest(100, DateOnly(2024, 1, 1))
          Interest(150, DateOnly(2024, 3, 1))
          Interest(50, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(interestFor transactions 2024, 300)

[<Test>]
let InterestIgnoresOtherTransactions () =
    let transactions =
        [ Interest(100, DateOnly(2024, 1, 1))
          Dividend("SPY", 50, DateOnly(2024, 2, 1))
          Interest(50, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(interestFor transactions 2024, 150)

[<Test>]
let InterestIgnoresOtherYears () =
    let transactions =
        [ Interest(75, DateOnly(2023, 12, 31))
          Interest(100, DateOnly(2024, 1, 1))
          Interest(50, DateOnly(2024, 4, 20))
          Interest(35, DateOnly(2025, 1, 15)) ]

    Assert.AreEqual(interestFor transactions 2024, 150)

[<Test>]
let DividendsSumOneSymbol () =
    let transactions =
        [ Dividend("SPY", 100, DateOnly(2024, 1, 1))
          Dividend("SPY", 150, DateOnly(2024, 3, 1))
          Dividend("SPY", 50, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(dividendsFor transactions 2024, Map<string, float> [ ("SPY", 300) ])

[<Test>]
let DividendsSumMultipleSymbols () =
    let transactions =
        [ Dividend("SPY", 100, DateOnly(2024, 1, 1))
          Dividend("SPY", 150, DateOnly(2024, 3, 1))
          Dividend("VOO", 50, DateOnly(2024, 4, 20))
          Dividend("VOO", 30, DateOnly(2024, 1, 6)) ]

    Assert.AreEqual(dividendsFor transactions 2024, Map<string, float> [ ("SPY", 250); ("VOO", 80) ])

[<Test>]
let DividendsIgnoresOtherTransactions () =
    let transactions =
        [ Dividend("SPY", 100, DateOnly(2024, 1, 1))
          Interest(25, DateOnly(2024, 2, 1))
          Dividend("SPY", 50, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(dividendsFor transactions 2024, Map<string, float> [ ("SPY", 150) ])

[<Test>]
let DividendsIgnoresOtherYears () =
    let transactions =
        [ Dividend("SPY", 100, DateOnly(2024, 1, 1))
          Dividend("SPY", 150, DateOnly(2024, 3, 1))
          Dividend("SPY", 50, DateOnly(2024, 4, 20))
          Dividend("SPY", 35, DateOnly(2025, 1, 15)) ]

    Assert.AreEqual(dividendsFor transactions 2024, Map<string, float> [ ("SPY", 300) ])

[<Test>]
let CapitalGainsBasicSell () =
    let transactions =
        [ Purchase("SPY", 24, 100, DateOnly(2024, 1, 1))
          Sell("SPY", 24, 110, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(capitalGainsFor transactions 2024, 240)

[<Test>]
let CapitalGainsBasicPartialSale () =
    let transactions =
        [ Purchase("SPY", 24, 100, DateOnly(2024, 1, 1))
          Sell("SPY", 12, 110, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(capitalGainsFor transactions 2024, 120)

[<Test>]
let CapitalGainsMultipleSell () =
    let transactions =
        [ Purchase("SPY", 100, 5, DateOnly(2024, 1, 1))
          Purchase("VOO", 100, 10, DateOnly(2024, 2, 1))
          Sell("SPY", 100, 10, DateOnly(2024, 4, 20))
          Sell("VOO", 100, 15, DateOnly(2024, 6, 10)) ]

    Assert.AreEqual(capitalGainsFor transactions 2024, 1000)

[<Test>]
let CapitalGainsMultipleLots () =
    let transactions =
        [ Purchase("SPY", 100, 5, DateOnly(2024, 1, 1))
          Purchase("SPY", 100, 10, DateOnly(2024, 2, 1))
          Sell("SPY", 200, 20, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(capitalGainsFor transactions 2024, 1500 + 1000)

[<Test>]
let CapitalGainsFifoLots () =
    let transactions =
        [ Purchase("SPY", 100, 5, DateOnly(2024, 1, 1))
          Purchase("SPY", 100, 10, DateOnly(2024, 2, 1))
          Sell("SPY", 150, 10, DateOnly(2024, 4, 20)) ]

    Assert.AreEqual(capitalGainsFor transactions 2024, 500 + 0)
