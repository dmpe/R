############################################################################
### Aufgabe 9

Produkte = read.table("Produkte.csv",
                      sep = ";",
                      dec = ",",
                      header = T)
(fit1 = lm(Bewertungen ~ Preissegment, data = Produkte))
summary(fit1)
anova(fit1)
summary(aov(Bewertungen ~ Preissegment, data = Produkte))
qqnorm(fit1$resid)
library(tseries)
jarque.bera.test(fit1$resid)
?  ? bartlett
bartlett.test(Bewertungen ~ Preissegment, data = Produkte)
TukeyHSD(aov(Bewertungen ~ Preissegment, data = Produkte), conf.level =
           0.99)
table(Produkte$Preissegment, Produkte$Verwendung)
levels(Produkte$Preissegment)
Produkte$Preissegment = relevel(Produkte$Preissegment, ref = "unteres")
levels(Produkte$Verwendung)
x_mu0 = rep(1, 300)
x_a1 = c(rep(1, 100), rep(0, 100), rep(-1, 100))
x_a2 = c(rep(0, 100), rep(1, 100), rep(-1, 100))
x_b1 = rep(c(rep(1, 50), rep(-1, 50)), 3)
x_ab11 = x_a1 * x_b1
x_ab21 = x_a2 * x_b1
(X = cbind(x_mu0, x_a1, x_a2, x_b1, x_ab11, x_ab21))
(X = model.matrix(
  ~ Preissegment * Verwendung,
  data = Produkte,
  contrasts.arg = list(Preissegment = "contr.sum", Verwendung = "contr.sum")
))
t(X) %*% X
solve(t(X) %*% X)
(kq.eff = solve(t(X) %*% X) %*% t(X) %*% Produkte$Bewertungen)
(mu = mean(Produkte$Bewertungen))
(a1 = mean(Produkte$Bewertungen[Produkte$Preissegment == "unteres"]) - mu)
boxplot(Produkte$Bewertungen)
boxplot(Bewertungen ~ Preissegment, data = Produkte)
boxplot(Bewertungen ~ Verwendung, data = Produkte)
boxplot(Bewertungen ~ Preissegment * Verwendung, data = Produkte)
fit2 = lm(Bewertungen ~ Preissegment * Verwendung, data = Produkte)
anova(fit2)
summary(lm(Produkte$Bewertungen ~ 0 + ., data = as.data.frame(X)))
interaction.plot(Produkte$Preissegment,
                 Produkte$Verwendung,
                 Produkte$Bewertung)
interaction.plot(Produkte$Verwendung,
                 Produkte$Preissegment,
                 Produkte$Bewertung)
