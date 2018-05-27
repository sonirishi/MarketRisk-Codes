rm(list=ls(all=T))

setwd("E:/Documents/Practice/Carol")

library(dplyr)
library(data.table)
library(Ecdat)

data("CRSPday")

return_data <- CRSPday[,4:7]

return_data_scaled <- as.data.frame(scale(return_data))

pr_return <- princomp(x = return_data_scaled, cor = F)

loadings <- pr_return$loadings

### Using only first 3 loadings ###########

ge_return <- as.matrix(pr_return$scores[,1:3]) %*% as.matrix(loadings[1,1:3])

ibm_return <- as.matrix(pr_return$scores[,1:3]) %*% as.matrix(loadings[2,1:3])

mobil_return <- as.matrix(pr_return$scores[,1:3]) %*% as.matrix(loadings[3,1:3])

crsp_return <- as.matrix(pr_return$scores[,1:3]) %*% as.matrix(loadings[4,1:3])

plot(density(return_data_scaled$ge),col="blue")
lines(density(ge_return),col="green")

plot(pr_return$scores[,1],type='l')

library(RDRToolbox)

lle_return <- RDRToolbox::LLE(as.matrix(return_data_scaled),dim = 3,k = 10)

lle_return <- RDRToolbox::Isomap(as.matrix(return_data_scaled),dim = 3,k = 10)


