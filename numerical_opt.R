rm(list=ls(all=T))
library(dplyr)

setwd("E:/Documents/Practice/Carol")

library(pracma)

yield <- function(y){
  cashflow = 92 - 5/(1+y) - 5/(1+y)^2 - 5/(1+y)^3 - 5/(1+y)^4 - 100/(1+y)^4
  return(cashflow)
}

pracma::newtonRaphson(yield, x0 = 0.001)  ## not sure why result is different

########### Personal Newton Raphson ###########

newton_method <- function(func,start = 0.001,maxiter=100,tol = 1.490116e-08){
  val = func(start)
  if(val >= -tol & val <= tol){
    return(c(val,0,start))
  }else{
    x = start
    i = 1
    while(i <= maxiter){
      x = x - func(x)/(grad(func,x) + 1e-12)   ## avoid division by 0
      val = func(x)
      if(val >= -tol & val <= tol){
        return(c(val,i,x))
      }else{
        i = i + 1
      }
    }
  }
  return(c(func(val),"NoConvergence","None"))
}  

newton_method(yield)
