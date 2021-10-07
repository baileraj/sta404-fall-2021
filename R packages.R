installed.packages()
currentRpkgsInstalled <- installed.packages()
str(currentRpkgsInstalled)

View(currentRpkgsInstalled)

as.vector(currentRpkgsInstalled[,"Package"])

paste(as.vector(currentRpkgsInstalled[,"Package"]),sep=",")

paste(c("a","b","c"),collapse=",")

paste(as.vector(currentRpkgsInstalled[,"Package"]),collapse=",")
