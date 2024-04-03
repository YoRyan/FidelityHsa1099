# FidelityHsa1099

A program to figure your Health Savings Account tax liability in California or New Jersey since Fidelity can't be arsed to do it for you.

To use it, supply your account activity exported as a csv file and the tax year you're requesting:

```
FidelityHsa1099 <path to csv> <year>
```
```
Gains Realized:		$ 61.75

Dividends Received:
VTI:			$ 49.71

Interest Earned:	$ 0.68
```

## Features

- [x] Interest earned
- [x] Dividends broken down by symbol
- [x] Capital gains using the LIFO method
- [x] Wash sale adjustments
- [ ] Long vs short term gains (not recognized by CA or NJ)
- [ ] Stock splits and reverse stock splits

## Disclaimer

The author is not a CPA. For tax advice, consult somebody who is.
