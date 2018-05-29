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
