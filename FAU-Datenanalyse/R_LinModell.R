############################################################################
### Aufgabe 6
Gehalt = c(5.1, 6.6, 4.5, 1.3, 0.5, 8)
ROE = c(.27, .2, .09, .1, .02, .13)
Umsatz = c(71, 67, 36, 5, 18, 103)
Sektor = c(1, 1, 0, 1, 0, 1)
(X = cbind(rep(1, 6), ROE, Umsatz, Sektor))
(XX = (t(X) %*% X))
(inv.XX = solve(t(X) %*% X))
(beta.dach = inv.XX %*% t(X) %*% Gehalt)
(eps.dach = Gehalt - X %*% beta.dach)
(var.eps.dach = sum(eps.dach ^ 2) / (6 - 3 - 1))
(beta2.dach = beta.dach[3])
(var.beta2.dach = var.eps.dach * inv.XX[3, 3])
(se.beta2.dach = sqrt(var.beta2.dach))
(t = beta2.dach / se.beta2.dach)
(k = qt(0.95, 6 - 3 - 1))
summary(lm(Gehalt ~ ROE + Umsatz + Sektor))


############################################################################
### Aufgabe 7
un = read.table(
  "Unternehmen.csv",
  sep = ";",
  dec = ",",
  header = T,
  row.names = 1
)
str(un)
colnames(un) = c("CEOGehalt", "Sektor" , "EBIT", "Umsatz", "ROE", "EKQuote")
which(is.na(un) == TRUE)
str(un)
summary(un)
(fit = summary(lm(
  CEOGehalt ~ ROE + Umsatz + Sektor + EBIT + EKQuote, data = un
)))
plot(density(fit$resid))
x = seq(-10, 10, by = 0.01)
lines(x, dnorm(x, mean(fit$resid), sd(fit$resid)), col = "red")
library(tseries)
jarque.bera.test(fit$resid)
un$SektorIuII.Dummy = as.numeric(un$Sektor == "IuII")
(fitoAeD = summary(
  lm(CEOGehalt ~ 0 + ROE + Umsatz + SektorIuII.Dummy + EBIT + EKQuote, data =
       un)
))
(fitoAbD = summary(lm(
  CEOGehalt ~ 0 + ROE + Umsatz + Sektor + EBIT + EKQuote, data = un
)))
un$FKQuote = 1 - un$EKQuote
(fit = summary(
  lm(CEOGehalt ~ ROE + Umsatz + Sektor + EBIT + EKQuote + FKQuote, data =
       un)
))
