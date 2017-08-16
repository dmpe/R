############################################################################
### Aufgabe 15

bewerber = read.table("Bewerber.txt",
                      header = T,
                      dec = ",",
                      sep = ";")
attach(bewerber)
plot(GMAT, GPA, col = Gruppe)
g1 = bewerber[Gruppe == 1, 1:2]
g2 = bewerber[Gruppe == 2, 1:2]
g3 = bewerber[Gruppe == 3, 1:2]
mu1 = colMeans(g1)
mu2 = colMeans(g2)
mu3 = colMeans(g3)
S1 = var(g1)
S2 = var(g2)
S3 = var(g3)
(Sp = ((nrow(g1) - 1) * S1 + (nrow(g2) - 1) * S2 + (nrow(g3) - 1) * S3) /
    (nrow(g1) + nrow(g2) + nrow(g3) - 3))
mu = (mu1 + mu2 + mu3) / 3
B = (mu1 - mu) %*% t(mu1 - mu) + (mu2 - mu) %*% t(mu2 - mu) + (mu3 - mu) %*%
  t(mu3 - mu)
(EW = eigen(solve(Sp) %*% B))
(DK = EW$vectors)
(Wilks = prod(1 / (1 + EW$values)))
(chi = -(nrow(bewerber) - (2 + 3) / 2 - 1) * log(Wilks))
qchisq(0.95, 2 * (3 - 1))
(d1 = t(mu1) %*% solve(Sp) %*% c(2.7, 530) - 1 / 2 * t(mu1) %*% solve(Sp) %*%
    mu1 + log(1 / 3))
(d2 = t(mu2) %*% solve(Sp) %*% c(2.7, 530) - 1 / 2 * t(mu2) %*% solve(Sp) %*%
    mu2 + log(1 / 3))
(d3 = t(mu3) %*% solve(Sp) %*% c(2.7, 530) - 1 / 2 * t(mu3) %*% solve(Sp) %*%
    mu3 + log(1 / 3))
library(MASS)
(Ergeb1 = lda(Gruppe ~ ., data = bewerber))
(Ergeb2 = lda(Gruppe ~ ., data = bewerber, prior = c(1 / 3, 1 / 3, 1 / 3)))
0.999998564 / 0.001694796 * (-0.008503148)
- 0.999969462 / 0.007815072 * (-0.01448967)
neu = data.frame(2.7, 530)
names(neu) = c("GPA", "GMAT")
predict(Ergeb1, neu)
predict(Ergeb2, neu)
lern = sample(1:85, 45)
bewerber[lern, ]
table(bewerber$Gruppe[lern])
Ergeb3 = lda(
  Gruppe ~ .,
  data = bewerber,
  prior = c(1 / 3, 1 / 3, 1 / 3),
  subset = lern
)
pred = predict(Ergeb3, bewerber[-lern, ])$class
wahr = bewerber$Gruppe[-lern]
(KM = table(pred, wahr))
(FK = 1 - (sum(diag(KM))) / sum(KM))
