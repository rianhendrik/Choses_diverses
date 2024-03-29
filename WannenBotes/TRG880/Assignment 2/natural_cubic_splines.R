#Natural cubic splines
library(openxlsx)
library(dplyr)
library(tidyverse)
library(splines)
library(ggplot2)

#setwd("/home/rian/Dropbox/2. TRG880/1. Assignments/Assignment 2")
setwd("C:/Users/rianh/Dropbox/2. TRG880/1. Assignments/Assignment 2")

d = read.xlsx("Heart_data.xlsx")

n = nrow(d)

d = d[,2:ncol(d)] %>%
  dplyr::select(-adiposity,-typea)

d$famhist=factor(d$famhist)




d_k = function(x, e, k, K){
  hockey1 = (x - e[k])^3 
  hockey2 = (x - e[K])^3
  hockey1[hockey1<0] <- 0
  hockey2[hockey2<0] <- 0
  denominator = e[K] - e[k] 
  return((hockey1 - hockey2)/denominator)
}



# Basis for sbp (h(x1))

x1 = d$sbp
e1 = unname(quantile(x1)) #knot values for sbp
K = 5 #two boundary knots and 3 interior knots



d1 = d_k(x1, e1, 1, K)
d2 = d_k(x1, e1, 2, K)
d3 = d_k(x1, e1, 3, K)
d4 = d_k(x1, e1, 4, K)

n1 = rep(1, length(x1)) #The global intercept, therefore not included in our basis h(X_sbp)
n2 = x1 #Basis function 1
n3 = d1 - d4 #Basis function 2
n4 = d2 - d4 #Basis function 3
n5 = d3 - d4 #Basis function 4


#Thus, h(X_sbp) is:

h_x1 = cbind(n2, n3, n4, n5)

########### Basis for tobaco (h(x2)) ##########

x2 = d$tobacco

e2 = unname(quantile(x2)) #knot values for sbp
K = 5 #two boundary knots and 3 interior knots



d1 = d_k(x2, e2, 1, K)
d2 = d_k(x2, e2, 2, K)
d3 = d_k(x2, e2, 3, K)
d4 = d_k(x2, e2, 4, K)


#n1 - global intercept
n2 = x2
n3 = d1 - d4
n4 = d2 - d4
n5 = d3 - d4

h_x2 = cbind(n2, n3, n4, n5)


########### Basis for ldl (h(x3)) ##########

x3 = d$ldl

e3 = unname(quantile(x3)) #knot values for sbp
K = 5 #two boundary knots and 3 interior knots



d1 = d_k(x3, e3, 1, K)
d2 = d_k(x3, e3, 2, K)
d3 = d_k(x3, e3, 3, K)
d4 = d_k(x3, e3, 4, K)


n2 = x3
n3 = d1 - d4
n4 = d2 - d4
n5 = d3 - d4

h_x3 = cbind(n2, n3, n4, n5)

########### Basis for famhist (h(x4)) ##########

h_x4 = as.numeric(d$famhist)

########### Basis for obesity (h(x5)) ##########

x5 = d$obesity

e5 = unname(quantile(x5)) #knot values for sbp
K = 5 #two boundary knots and 3 interior knots



d1 = d_k(x5, e5, 1, K)
d2 = d_k(x5, e5, 2, K)
d3 = d_k(x5, e5, 3, K)
d4 = d_k(x5, e5, 4, K)


n2 = x5
n3 = d1 - d4
n4 = d2 - d4
n5 = d3 - d4

h_x5 = cbind(n2, n3, n4, n5)


########### Basis for alcohol (h(c6)) ##########

x6 = d$alcohol

e6 = unname(quantile(x6)) #knot values for sbp
K = 5 #two boundary knots and 3 interior knots



d1 = d_k(x6, e6, 1, K)
d2 = d_k(x6, e6, 2, K)
d3 = d_k(x6, e6, 3, K)
d4 = d_k(x6, e6, 4, K)

n2 = x6
n3 = d1 - d4
n4 = d2 - d4
n5 = d3 - d4

h_x6 = cbind(n2, n3, n4, n5)

########### Basis for age (h(x7)) ##########

x7 = d$age

e7 = unname(quantile(x7)) #knot values for sbp
K = 5 #two boundary knots and 3 interior knots



d1 = d_k(x7, e7, 1, K)
d2 = d_k(x7, e7, 2, K)
d3 = d_k(x7, e7, 3, K)
d4 = d_k(x7, e7, 4, K)


n2 = x7
n3 = d1 - d4
n4 = d2 - d4
n5 = d3 - d4

h_x7 = cbind(n2, n3, n4, n5)




chd = as.numeric(d$chd)
y = chd

#x = data.matrix(data.frame(intercept = matrix(1, n, 1), h_x1, h_x2, h_x3, h_x4, h_x5, h_x6, h_x7))
x = data.matrix(data.frame(intercept = matrix(1, n, 1), h_x1, h_x2, h_x3, h_x4, h_x5, h_x7)) #Excluding alcohol
beta = matrix(0, nrow = ncol(x), ncol = 1)
prob = function(x, beta){
  return(1/(1+exp(-x%*%beta)))
}

x 
tol = 0.0001
crit = 1
counter = 0
while (crit > tol){
  counter = counter+1
  probs = prob(x, beta)
  W = diag(diag(probs%*%t((1-probs))), n, n)
  #score = t(x)%*%(y - probs)
  #hessian = -t(x)%*%W%*%x
  #beta_new = beta + solve(hessian)%*%score
  z = x%*%beta + solve(W)%*%(y-probs)
  beta_new = solve(t(x)%*%W%*%x)%*%t(x)%*%W%*%z
  crit = max(beta-beta_new)
  beta = beta_new
}

beta


####Covariance matrix of betas (SIGMA) ####

#inv(H`*W*H)

sigma = solve(t(x)%*%W%*%x)




#####Plots####

#sbp plot:
idx_sbp = c(2, 3, 4, 5)

x_sbp = x[,idx_sbp]
x_sbp = x_sbp[order(x_sbp[,1]),]

betas_sbp = as.matrix(unname(beta[idx_sbp,]))
yhat_sbp = x_sbp%*%betas_sbp


#plot(d$sbp, scaled.yhat_sbp)

#sbp variance plot:

idx_sbp = c(2, 3, 4, 5)
betas_sbp = as.matrix(unname(beta[idx_sbp,]))
yhat_sbp = x[,idx_sbp]%*%betas_sbp

sigma_sbp = sigma[2:5, 2:5]

var_sbp = x[,idx_sbp]%*%sigma_sbp%*%t(x[,idx_sbp])
pointwise_var_sbp = diag(var_sbp)
ci_upper_sbp = yhat_sbp + 2*sqrt(pointwise_var_sbp)
ci_lower_sbp = yhat_sbp - 2*sqrt(pointwise_var_sbp)

sbp_plot = data.frame(d$sbp, yhat_sbp, ci_lower_sbp, ci_upper_sbp)
sbp_plot = sbp_plot[order(sbp_plot$d.sbp),]


plot(sbp_plot[,1], sbp_plot[,2], type = "l")
lines(sbp_plot[,1], sbp_plot[,4], col = "red")
lines(sbp_plot[,1], sbp_plot[,3], col = "blue")

ggplot(sbp_plot, aes(sbp_plot[,1])) + 
  geom_line(aes(y=sbp_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=sbp_plot[,3], ymax=sbp_plot[,4]), alpha=0.2)


#tobacco plot

idx_tob = c(6, 7, 8, 9)
betas_tob = as.matrix(unname(beta[idx_tob,]))
yhat_tob = x[,idx_tob]%*%betas_tob

sigma_tob = sigma[6:9, 6:9]

var_tob = x[,idx_tob]%*%sigma_tob%*%t(x[,idx_tob])
pointwise_var_tob = diag(var_tob)
ci_upper_tob = yhat_tob + 2*sqrt(pointwise_var_tob)
ci_lower_tob = yhat_tob - 2*sqrt(pointwise_var_tob)

tob_plot = data.frame(d$tobacco, yhat_tob, ci_lower_tob, ci_upper_tob)
tob_plot = tob_plot[order(tob_plot$d.tobacco),]


plot(tob_plot[,1], tob_plot[,2], type = "l")
lines(tob_plot[,1], tob_plot[,4], col = "red")
lines(tob_plot[,1], tob_plot[,3], col = "blue")

ggplot(tob_plot, aes(tob_plot[,1])) + 
  geom_line(aes(y=tob_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=tob_plot[,3], ymax=tob_plot[,4]), alpha=0.3)


#ldl plot


idx_ldl = c(10, 11, 12, 13)
betas_ldl = as.matrix(unname(beta[idx_ldl,]))
yhat_ldl = x[,idx_ldl]%*%betas_ldl

sigma_ldl = sigma[10:13, 10:13]

var_ldl = x[,idx_ldl]%*%sigma_ldl%*%t(x[,idx_ldl])
pointwise_var_ldl = diag(var_ldl)
ci_upper_ldl = yhat_ldl + 2*sqrt(pointwise_var_ldl)
ci_lower_ldl = yhat_ldl - 2*sqrt(pointwise_var_ldl)

ldl_plot = data.frame(d$ldl, yhat_ldl, ci_lower_ldl, ci_upper_ldl)
ldl_plot = ldl_plot[order(ldl_plot$d.ldl),]


plot(ldl_plot[,1], ldl_plot[,2], type = "l")
lines(ldl_plot[,1], ldl_plot[,4], col = "red")
lines(ldl_plot[,1], ldl_plot[,3], col = "blue")

ggplot(ldl_plot, aes(ldl_plot[,1])) + 
  geom_line(aes(y=ldl_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=ldl_plot[,3], ymax=ldl_plot[,4]), alpha=0.2)


# plot obesity

idx_ob = c(15, 16, 17, 18)
betas_ob = as.matrix(unname(beta[idx_ob,]))
yhat_ob = x[,idx_ob]%*%betas_ob

sigma_ob = sigma[15:18, 15:18]

var_ob = x[,idx_ob]%*%sigma_ob%*%t(x[,idx_ob])
pointwise_var_ob = diag(var_ob)
ci_upper_ob = yhat_ob + 2*sqrt(pointwise_var_ob)
ci_lower_ob = yhat_ob - 2*sqrt(pointwise_var_ob)

ob_plot = data.frame(d$obesity, yhat_ob, ci_lower_ob, ci_upper_ob)
ob_plot = ob_plot[order(ob_plot$d.obesity),]


plot(ob_plot[,1], ob_plot[,2], type = "l")
lines(ob_plot[,1], ob_plot[,4], col = "red")
lines(ob_plot[,1], ob_plot[,3], col = "blue")

ggplot(ob_plot, aes(ob_plot[,1])) + 
  geom_line(aes(y=ob_plot[,2]), colour="green") + 
  geom_ribbon(aes(ymin=ob_plot[,3], ymax=ob_plot[,4]), alpha=0.3)
















  # Make the Pairs Plot
pairs(d[1:7],col=as.factor(d$chd))

#Fitting a logistic regression model to the data
heartModel=glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family='binomial',data=d)
summary(heartModel)

#Fitting a natural spline to the data

format = "chd ~ ns(sbp,df=4) + ns(tobacco,df=4) + ns(ldl,df=4) + famhist + ns(obesity,df=4) + ns(alcohol,df=4) + ns(age,df=4)"
format = formula(format)

?glm
splineModel = glm( format, data=d, family=binomial )
print( summary(splineModel), digits=3 )

drop1(splineModel, scope=format, test="Chisq" )

backstep2 = step(splineModel) # Backwards selection is the default
summary(backstep2)
drop1( backstep2, test="Chisq" )

splineModel$coefficients
length(splineModel$coefficients)

#Plot for sbp
sbp = d$sbp

e1 = 0.4
e2 = 0.6
e4 = max(sbp)





theta1 = -1.47936736 #beta1
theta2 = -1.351182 #beta2
theta3 = -3.75372259#theta3
theta4 = 1.39731908#theta4

thetaprime1 = theta1*(e4 - e1)
thetaprime2 = theta2*(e4 - e2)
thetaprime3 = theta3*(e4 - e3)
thetaprime4 = theta4*(e4 - e4)

dk3 = ((sbp - e3)^3 - (sbp - e4)^3)/(e4-e3)
dk4 = 0

n1 = 1
n2 = sbp
n3 = dk3 - dk3
n4 = dk4 - dk3


f.sbp =  thetaprime1*n1 + thetaprime2*n2 + thetaprime3*n3 + thetaprime4*n4


plot(sbp, f.sbp)
