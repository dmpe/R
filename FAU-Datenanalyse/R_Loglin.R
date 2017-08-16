#############################################################################
### Aufgaben 17.10/17.11

net = read.table("net.csv", sep = ";", header = T)
str(net)
(t3 = table(net$Art, net$Nutzer, net$Sprache))
(t3 = xtabs( ~ Art + Nutzer + Sprache, data = net))
(h3 = as.vector(t3))
(Faktoren3 = expand.grid(
  Art = c("beruflich", "privat"),
  Nutzer = c("<=5", ">5"),
  Sprache = c("eine", "mehrere")
))
(net.h3 = data.frame(Faktoren3, h3))
library(MASS)
M3 = loglm(h3 ~ (Art + Nutzer + Sprache) ^ 3,
           data = net.h3,
           fitted = T)
M3 = loglm(h3 ~ Art * Nutzer * Sprache, data = net.h3, fitted = T)
M3 = loglm(
  h3 ~ Art + Nutzer + Sprache + Nutzer * Art + Nutzer * Sprache + Art * Sprache +
    Art * Nutzer * Sprache,
  data = net.h3,
  fitted = T
)
M3.glm = glm(
  h3 ~ (Art + Nutzer + Sprache) ^ 3,
  data = net.h3,
  family = poisson(log),
  x = T,
  contrasts = list(
    Art = "contr.sum",
    Nutzer = "contr.sum",
    Sprache = "contr.sum"
  )
)
coef(M3)
M3$para
fitted(M3)
M3$fitted
M3.glm$fitted
M3.glm$x
M3.glm$coef
M2 = loglm(h3 ~ (Art + Nutzer + Sprache) ^ 2,
           data = net.h3,
           fitted = T)
M2 = loglm(
  h3 ~ Art + Nutzer + Sprache + Art * Nutzer + Art * Sprache + Nutzer * Sprache,
  data = net.h3,
  fitted = T
)
M2.glm = glm(
  h3 ~ Nutzer + Art + Sprache + Nutzer:Art + Nutzer:Sprache + Art:Sprache,
  data = net.h3,
  family = poisson(log),
  contrasts = list(
    Art = "contr.sum",
    Nutzer = "contr.sum",
    Sprache = "contr.sum"
  )
)
M.AC.BC = loglm(
  h3 ~ Art + Nutzer + Sprache + Art * Sprache + Nutzer * Sprache,
  data = net.h3,
  fitted = T
)
M.AC.BC.glm = glm(
  h3 ~ Nutzer + Art + Sprache + Art * Sprache + Nutzer * Sprache,
  data = net.h3,
  family = poisson(log),
  contrasts = list(
    Art = "contr.sum",
    Nutzer = "contr.sum",
    Sprache = "contr.sum"
  )
)
M1 = loglm(h3 ~ Art + Nutzer + Sprache, data = net.h3, fitted = T)
M1.glm = glm(
  h3 ~ Nutzer + Art + Sprache,
  data = net.h3,
  family = poisson(log),
  contrasts = list(
    Art = "contr.sum",
    Nutzer = "contr.sum",
    Sprache = "contr.sum"
  )
)
2 * sum(t3 * log(M3$fitted / M2$fitted))
M2$lrt - M3$lrt
qchisq(0.95, M2$df - M3$df)
2 * sum(t3 * log(M2$fitted / M.AC.BC$fitted))
M.AC.BC$lrt - M2$lrt
qchisq(0.95, M.AC.BC$df - M2$df)
t3
10 * 2 / (4 * 1)
5 * 11 / (3 * 4)