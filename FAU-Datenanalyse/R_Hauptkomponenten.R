############################################################################
### Aufgabe 11

staedte = read.table("staedte.txt", header = T)
str(staedte)
(S.staedte = cov(staedte))
(R.staedte = cor(staedte))
diag(S.staedte)
(EW.staedte = eigen(R.staedte)$values)
cumsum(EW.staedte) / length(EW.staedte)
plot(EW.staedte, type = "o")
(EV.staedte = eigen(R.staedte)$vectors)
staedte.stand = scale(staedte)
colMeans(staedte.stand)
staedte.stand = sweep(staedte, 2, STAT = colMeans(staedte), FUN = "-")
staedte.stand = sweep(staedte.stand, 2, STAT = sqrt(diag(cov(staedte))), FUN =
                        "/")
staedte.stand = as.matrix(staedte.stand)
cov(staedte.stand)
(HK1.staedte = staedte.stand %*% EV.staedte[, 1])
(HK.staedte = summary(princomp(staedte.stand, scores = TRUE)))
HK.staedte$scores
HK.staedte$scores[, 1]