#############################################################################
### Aufgabe 16.4

users = read.table("users.txt")
dist(users)
users_m = t(matrix(
  rep(colMeans(users), nrow(users)),
  nrow = ncol(users),
  byrow = F
))
s = (1 / nrow(users) * (colSums(abs(users - users_m) ^ 2))) ^ (1 / 2)
users_s = matrix(rep(s, nrow(users)), nrow = nrow(users), byrow = T)
users_norm = (users - users_m) / users_s
users_norm = sweep(users, 2, STAT = colMeans(users), FUN = "-")
users_norm = sweep(users_norm, 2, STAT = sqrt(((nrow(
  users
) - 1) / nrow(users)) * diag(cov(users))), FUN = "/")
users_norm = scale(users) * sqrt(nrow(users) / (nrow(users) - 1))
distanz = dist(users_norm, method = "minkowski", p = 2)
ave = hclust(distanz, method = "average")
ave$height
ave$merge
single = hclust(distanz, method = "single")
comp = hclust(distanz, method = "complete")
ward = hclust(distanz, method = "ward.D2")
par(mfrow = c(2, 2))
plot(ave)
plot(single)
plot(comp)
plot(ward)
KM = kmeans(users, 3)
KM$cluster
KM$centers
par(mfrow = c(1, 1))
plot(users[, 1], users[, 3], col = KM$cluster)
points(
  KM$centers[, 1],
  KM$centers[, 3],
  col = 1:3,
  pch = 2,
  cex = 2
)
text(users[, 1], users[, 3], lab = 1:20, col = KM$cluster)
