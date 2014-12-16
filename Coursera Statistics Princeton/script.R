library(Quandl)
Quandl.auth("GgnxpyU***bY")
# Employment numbers (thousands of people") for Lansing, Michigan
NonFarm = Quandl("FRED/LANS626NAN")
CivLaborForce = Quandl("FRED/LANS626LFN")
PerCapitaIncome = Quandl("FRED/LANS626PCPI")

# Now let's combine the data so that we can related data values.
Labor = merge(NonFarm, CivLaborForce, by="Date")
Combined = merge(Labor, PerCapitaIncome, by="Date")
colnames(Combined) = c("Date", "NonFarm", "CivLaborForce", "PerCapitaIncome")

# Let's see if we can predict income as a function of employment:
summary(lm(PerCapitaIncome~NonFarm+CivLaborForce, data=Combined))
