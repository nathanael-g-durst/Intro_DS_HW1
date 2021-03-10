#Adel's commit

library(quantmod)
library(zoo)

# (2) Fetch data regarding Apple (AAPL), Cisco (CSCO), Intel Corporation (INTC), Home Depot (HD), Alphabet (GOOG), and JPMorgan Chase (JPM) stocks in the period between 2020-01-01 to 2021-01-01 using the package quantmod.

ticker <- c("AAPL", "CSCO", "INTC", "HD", "GM", "GOOG", "JPM")
getSymbols.yahoo(ticker, env=globalenv(), from="2020-01-01", to = "2021-01-01")

#(3) Plot the closing price of AAPL and CSCO over the period between 2020-01-01 to 2021-01-01.

data.raw <- merge.zoo(Cl(AAPL), Cl(CSCO))
plot(data.raw)

#(4) Plot a candle chart of GOOG and JPM over the last 3 months of 2020.

ticker1 <- c("GOOG", "JPM")
getSymbols.yahoo(ticker1, env=globalenv(), from="2020-10-01", to = "2020-12-31")
data.raw1 <- merge.zoo(Cl(AAPL), Cl(CSCO))
candleChart(data.raw1)
