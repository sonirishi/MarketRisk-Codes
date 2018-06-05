print(pnorm(0,0.1,0.2))  # using CDF

X <- 1
Y <- 3

corr <- 0.5

EX <- .24; EVX <- 0.2
EY <- .16; EVY <- 0.1

wx <- X/(X+Y)

wy <- 1-wx

EP <- wx*EX + wy*EY

EVP <- sqrt(wx^2*0.2^2 + wy^2*EVY^2 + 2*0.5*wx*wy*EVX*EVY)

######## returns normal as sum of normal is normal

print(pnorm(0,EP,EVP))  # using CDF

print(1-pnorm(0.2,EP,EVP))  # Probability of return achieved greater than 0.2

############

corr <- 0.75
EP <- 0.1
EB <- 0.08

VP <- 0.25; VB <- 0.15

### Think as if long fund and short benchmark so active

EF <- EP - EB
VF <- sqrt(VP^2 + VB^2 - 2*VP*VB*corr)

print(pnorm(0,EF,VF))

##### Mixture of normal 

norm1 <- rnorm(1000,0,1)
norm2 <- rnorm(1000,0,4)

iter <- 1000

norm3 <- rep(0,1000)

for(i in 1:iter){
  toss <- runif(1,0,1)
  norm3[i] <- ifelse(toss >= 0.3,rnorm(1,0,1),rnorm(1,0,4))
}

plot(density(norm1),col="red")
lines(density(norm2),col="green")
lines(density(norm3),col="black")

############ Want to do using simulation

library(moments)

V1 <- 0.03
V2 <- 0.04
p <- 0.5

iter <- 1000
vals <- 1000

norm3 <- rep(0,vals)

store_std <- rep(0,iter); store_kurt <- rep(0,iter)

for(z in 1:iter){
  for(i in 1:vals){
    toss <- runif(1,0,1)
    norm3[i] <- ifelse(toss >= p,rnorm(1,0,V1),rnorm(1,0,V2))
  }
  store_std[z] <- sd(norm3)
  store_kurt[z] <- moments::kurtosis(norm3)
}

print(mean(store_std))
print(mean(store_kurt))

############# Standard error

print(sd(store_std))
print(sd(store_kurt))

##########

EX <- 1
VX <- 4

EY <- -2
VY <- 5
p <- 0.25

p*pnorm(-1,EX,VX) + (1-p)*pnorm(-1,EY,VY)

##############

library(mvtnorm)

p1 <- 0.3
p2 <- 0.5

print(p1*p1 + (1-p1)*(1-p2) + p2*(1-p1) + p1*(1-p2))

######### Sample Test on Covariance ##############

library(Ecdat)
data("CRSPday")

return_data <- as.data.frame(CRSPday[,4:7])

return_data$final <- 0.25*return_data$ge + 0.25*return_data$ibm + 0.25*return_data$mobil + 
  0.25*return_data$crsp

print(sd(return_data$final))

cov_matrix <- cov(return_data[,1:4])

eigen(cov_matrix)  # positive definite as all eigenvalues are positive

sd_ind <- sqrt(0.25^2*(var(return_data$ge) + var(return_data$ibm) + var(return_data$mobil) + 
  var(return_data$crsp) + 2*cov(return_data$ge,return_data$ibm) + 
    2*cov(return_data$ge,return_data$mobil) + 2*cov(return_data$ge,return_data$crsp) + 
    2*cov(return_data$ibm,return_data$mobil) + 2*cov(return_data$ibm,return_data$crsp) + 
    2*cov(return_data$mobil,return_data$crsp)))

##### Hence Proved that we dont really need to look at individual stocks in case of portfolio
