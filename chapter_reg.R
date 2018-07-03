rm(list=ls(all=T))
library(dplyr)
library(quantmod)

setwd("E:/Documents/Practice/Carol")

start <- as.Date("2000-01-06")
end <- as.Date("2007-12-31")

getSymbols("BHP", src = "yahoo", from = start, to = end)

getSymbols("XOI", src = "yahoo", from = start, to = end)

getSymbols("CNX", src = "yahoo", from = start, to = end)

bhp_data1 <- as.data.frame(BHP)
bhp_data1$date <- rownames(bhp_data1)
xoi_data1 <- as.data.frame(XOI)
xoi_data1 <- na.locf(xoi_data1,fromLast = T)
xoi_data1$date <- rownames(xoi_data1)
cnx_data1 <- as.data.frame(CNX)
cnx_data1$date <- rownames(cnx_data1)

final_data <- merge(bhp_data1,xoi_data1,by.x = "date",by.y = "date",all = F)

final_data_1 <- merge(final_data,cnx_data1,by.x = "date",by.y = "date",all = F)

saveRDS(final_data_1,"price_data_prac.rds")

##############################################################
#############################################################

price_data_prac <- readRDS("price_data_prac.rds")

reg_data <- price_data_prac[,c("BHP.High","XOI.High","CNX.High")]

reg_data <- reg_data %>% mutate(bhp_return = (BHP.High - lag(BHP.High))/lag(BHP.High),
                                xoi_return = (XOI.High - lag(XOI.High))/lag(XOI.High),
                                cnx_return = (CNX.High - lag(CNX.High))/lag(CNX.High))

y <- as.matrix(reg_data[-1,"bhp_return"])

x <- cbind(1,as.matrix(reg_data[-1,c("xoi_return","cnx_return")]))  ## for the intercept term

xprimex <- solve(t(x)%*%x)

xprimey <- t(x)%*%y

beta_matrix <- xprimex%*%xprimey

lm_model <- lm(y~x)

print(summary(lm_model))

RSS <- t(y) %*% y - t(beta_matrix) %*% t(x) %*% y

estimate_variance <- RSS/(dim(y)[1] - 2)

variance_beta <- estimate_variance[1]*xprimex  ## sqrt of diagonal is standard error

eigen(variance_beta)   ## postive eigen hence positive definite matrix

########

beta <- matrix(c(0.05,0.97,-0.55,1.23),4,1)

var_beta <- matrix(c(0.005,-0.001,0.002,0.003,-0.001,0.04,0.005,0.003,0.002,0.005,0.03,
                     -0.003,0.003,0.003,-0.003,0.05),byrow=F,4,4)

sum_coef <- matrix(c(1,1,-1,-1),4,1)

variance_sum <- t(sum_coef) %*% var_beta %*% sum_coef  ## w'*covar*w

t_stat <- (t(sum_coef) %*% beta)/sqrt(variance_sum)  ## Data has 64 obs and 4 variables hence 60 is DF

#########

vector_change <- c(1,-0.1,-0.2)

std_err_y_chg <- estimate_variance[1] + (t(vector_change) %*% variance_beta %*% vector_change)[1,1]

saveRDS(reg_data,"reg_data.rds")

#############3

reg_data <- readRDS("reg_data.rds")

y <- as.matrix(reg_data[-1,"bhp_return"])

x <- cbind(1,as.matrix(reg_data[-1,c("xoi_return","cnx_return")]))  ## for the intercept term

lm_model <- lm(y~x)

error <- lm_model$residuals

library(car)

db_test <- durbinWatsonTest(lm_model)

plot(error)

plot(density(error))

qqnorm(error,datax = T)
qqline(error,datax = T)

library(moments)

kurtosis(error)  ## more than 3 

library(lmtest)

bp_test <- lmtest::bptest(lm_model)

ncv_test <- car::ncvTest(lm_model)
