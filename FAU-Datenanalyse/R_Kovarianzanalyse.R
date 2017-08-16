############################################################################
### Aufgabe 10

Produkte2 = read.table("Produkte2.csv",
                       sep = ";",
                       dec = ",",
                       header = T)
(fit3 = lm(Bewertungen ~ Preissegment * Verwendung + Anzahl, data = Produkte2))
anova(fit3)
(fit4 = lm(
  Bewertungen ~ Preissegment * Verwendung + Preissegment * Anzahl,
  data = Produkte2
))
anova(fit4)
summary(fit4)