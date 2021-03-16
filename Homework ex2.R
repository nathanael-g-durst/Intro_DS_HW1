library(quantmod)

ticker <- c("AAPL", "CSCO", "INTC", "HD", "GOOG", "JPM")
getSymbols.yahoo(ticker, env=globalenv(), from="2011-01-01", to = "2021-01-01")


rf = 0.03/252

# daily returns

AAPL_return = dailyReturn(AAPL)
CSCO_return = dailyReturn(CSCO)
INTC_return = dailyReturn(INTC)
HD_return = dailyReturn(HD)
GOOG_return = dailyReturn(GOOG)
JPM_return = dailyReturn(JPM)

#excess returns

AAPL_ex_return = AAPL_return - rf
CSCO_ex_return = CSCO_return - rf
INTC_ex_return = INTC_return - rf
HD_ex_return = HD_return - rf
GOOG_ex_return = GOOG_return - rf
JPM_ex_return = JPM_return - rf

meanAAPL = apply(AAPL_return, 2, mean)
varAAPL = apply(AAPL_return, 2, var)
sdAAPL = apply(AAPL_return, 2, sd)

AAPL_yearly_sharpe = mean(AAPL_ex_return)/sdAAPL[1]*sqrt(252)

meanCSCO = apply(CSCO_return, 2, mean)
varCSCO = apply(CSCO_return, 2, var)
sdCSCO = apply(CSCO_return, 2, sd)

CSCO_yearly_sharpe = mean(CSCO_ex_return)/sdCSCO[1]*sqrt(252)

meanINTC = apply(INTC_return, 2, mean)
varINTC = apply(INTC_return, 2, var)
sdINTC = apply(INTC_return, 2, sd)

INTC_yearly_sharpe = mean(INTC_ex_return)/sdINTC[1]*sqrt(252)

meanHD = apply(HD_return, 2, mean)
varHD = apply(HD_return, 2, var)
sdHD = apply(HD_return, 2, sd)

HD_yearly_sharpe = mean(HD_ex_return)/sdHD[1]*sqrt(252)

meanGOOG = apply(GOOG_return, 2, mean)
varGOOG = apply(GOOG_return, 2, var)
sdGOOG = apply(GOOG_return, 2, sd)

AAPL_yearly_sharpe = mean(GOOG_ex_return)/sdGOOG[1]*sqrt(252)

meanJPM = apply(JPM_return, 2, mean)
varJPM = apply(JPM_return, 2, var)
sdJPM = apply(JPM_return, 2, sd)

JPM_yearly_sharpe = mean(JPM_ex_return)/sdJPM[1]*sqrt(252)

yearly_sharpe = cbind(AAPL_yearly_sharpe, CSCO_yearly_sharpe, CSCO_yearly_sharpe, INTC_yearly_sharpe, HD_yearly_sharpe, JPM_yearly_sharpe)
colnames(yearly_sharpe) = c("AAPL", "CSCO", "INTC", "HD", "GOOG", "JPM")
yearly_sharpe


# Question 7

mat_return = cbind(AAPL_return,CSCO_return, INTC_return, HD_return, GOOG_return, JPM_return)
colnames(mat_return)= c("AAPL", "CSCO", "INTC", "HD", "GOOG", "JPM")

mu = apply(mat_return, 2, mean)
Sigma <- var(mat_return)

# equally weighted portfolio
equal_return = (AAPL_return + CSCO_return + INTC_return + HD_return + GOOG_return + JPM_return)/6

mu_equal = mean(equal_return)
var_equal = var(equal_return)

#  stocks variances
diag(Sigma)  


# Question 8

var = rep(1,6)
inv_Sigma = solve(Sigma)
P = as.numeric( inv_Sigma %*% var / as.numeric(t(var) %*% inv_Sigma %*% var))

optimum_return = AAPL_return*P[1] + CSCO_return*P[2] + INTC_return*P[3] + HD_return*P[4] + GOOG_return*P[5] + JPM_return*P[6]


# Compute omega^*
omega_star <- (Sigma[2, 2] - Sigma[1, 2])/(Sigma[1, 1] + Sigma[2, 2] - 2*Sigma[1, 2])

# Compute investment expected value and variance
mu_investment <- omega_star*mu[1] + (1 - omega_star)*mu[2]
var_investment <- omega_star^2*Sigma[1,1] + (1 - omega_star)^2*Sigma[2,2] + 
  2*omega_star*(1 - omega_star)*Sigma[1,2]

investment_summary <- matrix(NA, 2, 7)
dimnames(investment_summary)[[1]] <- c("Expected value", "Variance")
dimnames(investment_summary)[[2]] <- c("AAPL", "CSCO", "INTC", "HD", "GOOG", "JPM", "Investment")
investment_summary[1, ] <- c(mu, mu_investment)
investment_summary[2, ] <- c(diag(Sigma), var_investment)
knitr::kable(investment_summary)

plot((sqrt(investment_summary[2, ])), investment_summary[1, ],
     main="Stocks Portfolio",
     xlab="Daily Investment Standard Deviation",
     ylab="Daily Expected Value of Investment ",
     xlim = c(min(sqrt(investment_summary[2, ])), max(sqrt(investment_summary[2, ])+1e-04)),
     ylim = c(0, 0.0012),
     lty = "solid",
     lwd = 2, 
     col = 1:8,
     cex = 1,
     pch = 19,
grid())
text(sqrt(investment_summary[2, ]), investment_summary[1, ], labels = names(investment_summary[2, ]), cex = 0.7, pos = 3)


