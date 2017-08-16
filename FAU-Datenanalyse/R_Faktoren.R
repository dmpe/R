############################################################################
### Aufgabe 13

stkor = as.matrix(read.table("korrelationen.txt", header = T))
(EW = eigen(stkor)$values)
bartlett.faktor = function(n, e, m, alpha) {
  p = length(e)
  e.mp = e[(m + 1):p]
  Ts = (n - (2 * p + 11) / 6) * ((p - m) * log(mean(e.mp)) - sum(log(e.mp)))
  fhg = (p - m + 2) * (p - m - 1) / 2
  k = qchisq(1 - alpha, fhg)
  Erg = ifelse(Ts > k, "H0 ablehnen", "H0 nicht ablehnen")
  out = list(Ts, k, Erg)
  return(out)
}
bartlett.faktor(100, EW, 1, 0.05)
EV = eigen(stkor)$vectors
L1 = as.matrix(sqrt(EW[1]) * EV[, 1], ncol = 1)
abw = stkor - (L1 %*% t(L1))
diag(abw) = rep(0, 5)
max(abs(abw))
max(abs(abw[upper.tri(abw)]))
(Ladung.HK = cbind(sqrt(EW[1]) * EV[, 1], sqrt(EW[2]) * EV[, 2]))
(ML = factanal(
  covmat = as.matrix(stkor),
  factors = 2,
  rotation = "none"
))
(Ladung.ML = ML$loadings)
loadings(ML)
h.HK = rowSums(Ladung.HK ^ 2)
h.ML = rowSums(Ladung.ML ^ 2)
cbind(h.HK, h.ML)
anVar.HK = colSums(Ladung.HK ^ 2) / nrow(stkor)
anVar.ML = colSums(Ladung.ML ^ 2) / nrow(stkor)
cbind(anVar.HK, anVar.ML)
apply(cbind(anVar.HK, anVar.ML), MARGIN = 2, FUN = cumsum)
(ML.var = varimax(Ladung.ML))
factanal(covmat = as.matrix(stkor),
         factors = 2,
         rotation = "varimax")
Ladung.ML %*% ML.var$rotmat
(HK.var = varimax(Ladung.HK))
Ladung.ML.var = ML.var$loadings
Ladung.HK.var = HK.var$loadings
par(mfrow = c(2, 2))
plot(
  Ladung.ML[, 1],
  Ladung.ML[, 2],
  type = "n",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  xaxs = "i",
  yaxs = "i"
)
text(Ladung.ML[, 1], Ladung.ML[, 2], labels = 1:5, col = 2)
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
plot(
  Ladung.HK[, 1],
  Ladung.HK[, 2],
  type = "n",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  xaxs = "i",
  yaxs = "i"
)
text(Ladung.HK[, 1], Ladung.HK[, 2], labels = 1:5, col = 4)
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
plot(
  Ladung.ML.var[, 1],
  Ladung.ML.var[, 2],
  type = "n",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  xaxs = "i",
  yaxs = "i"
)
text(Ladung.ML.var[, 1],
     Ladung.ML.var[, 2],
     labels = 1:5,
     col = 4)
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
plot(
  Ladung.HK.var[, 1],
  Ladung.HK.var[, 2],
  type = "n",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  xaxs = "i",
  yaxs = "i"
)
text(Ladung.HK.var[, 1],
     Ladung.HK.var[, 2],
     labels = 1:5,
     col = 3)
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
