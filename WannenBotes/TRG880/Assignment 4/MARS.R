#Assignment 4 - MARS
wd_linux = "/home/rian/Dropbox/2. TRG880/1. Assignments/Assignment 4"
wd_windows = "C:/Users/rianh/Dropbox/2. TRG880/1. Assignments/Assignment 4"
setwd(wd_linux)
library(scatterplot3d)
d = openxlsx::read.xlsx("ex3.xlsx", cols = c(1, 2,3))


n = nrow(d)
p = ncol(d)-1
#Surface plot of y against x1 and x2

q1plot = scatterplot3d(d$x1, d$x2, d$y, color = "blue", pch = 16, type = "h",
                       xlab = "X1",
                       ylab = "X2",
                       zlab = "Y",
                       main = "3D scatterplot of the data")

#Surface plot of y against x1 and x2
q1plot = scatterplot3d(d$x1, d$x2, d$y, color = "blue", pch = 16, type = "h",
                       xlab = "X1",
                       ylab = "X2",
                       zlab = "Y",
                       main = "3D scatterplot of the data (including multiple linear regression plane)")

data.lm <- lm(y ~ x1 + x2, data=d)
q1plot$plane3d(data.lm)
summary(data.lm)


m.sses = matrix(NA, nrow = 4)

tss = t((d$y - mean(d$y)))%*%(d$y - mean(d$y))



#First MARS step: Fitting an intercept

intercept = matrix(1, nrow = nrow(d))
x = intercept
thetas = solve(t(x)%*%x)%*%t(x)%*%d$y
yhat = thetas[1]*x
sse = t(d$y-yhat)%*%(d$y-yhat)
m.sses[1] = sse

#Second MARS step: Determining the next best variable and knot
sses = matrix(NA, nrow = n*p, ncol = 3)
counter = 0

for (i in 1:p){
  var = sort(d[,i])
  for (j in 1:n){
    counter = counter+1
    e1 = var[j]
    x = cbind(intercept, h1 = ifelse(var < e1, var, 0), 
                         h2 = ifelse(var > e1, var, 0)) 
    if (det(t(x)%*%x) != 0){yhat = x%*%solve(t(x)%*%x)%*%t(x)%*%d$y
                    sses[counter,] = c(t(d$y-yhat)%*%(d$y-yhat), i, j)}
  }
}


opk = which(sses[,1] == min(sses[,1], na.rm=T))
sses[opk,]
m.sses[2] = sses[opk, 1]


# Third MARS step

sses_2 = matrix(NA, nrow = n*p, ncol = 3)
counter = 0

for (i in 1:p){
  var = sort(d[,i])
  for (j in 1:n){
    counter = counter+1
    e1 = var[j]
    x = cbind(intercept, x2.1 = ifelse(d$x2 < d$x2[sses[opk,3]], d$x2, 0), 
              x2.2 = ifelse(d$x2 > d$x2[sses[opk,3]], d$x2, 0),
              h1 = ifelse(var < e1, var, 0), 
              h2 = ifelse(var > e1, var, 0)) 
    if (det(t(x)%*%x) != 0){yhat = x%*%solve(t(x)%*%x)%*%t(x)%*%d$y
    sses_2[counter,] = c(t(d$y-yhat)%*%(d$y-yhat), i, j)}
  }
}



opk_2 = which(sses_2[,1] == min(sses_2[,1], na.rm=T))
sses_2[opk_2,]
m.sses[3] = sses_2[opk_2, 1]





# Fourth MARS step (first interaction term)

sses_3 = matrix(NA, nrow = n*p, ncol = 3)
counter = 0

for (i in 1:p){
  var = sort(d[,i])
  for (j in 1:n){
    counter = counter+1
    e1 = var[j]
    x2.1 = ifelse(d$x2 < d$x2[sses[opk,3]], d$x2, 0)
    x2.2 = ifelse(d$x2 > d$x2[sses[opk,3]], d$x2, 0)
    x3.1 = ifelse(d$x2 < d$x2[sses_2[opk_2,3]], d$x2, 0)
    x3.2 = ifelse(d$x2 > d$x2[sses_2[opk_2,3]], d$x2, 0)
    x = cbind(intercept, x2.1, x2.2, x3.1, x3.2,
              h1 = (x3.1 * ifelse(var < e1, var, 0)), 
              h2 = (x3.1 * ifelse(var > e1, var, 0)))
    if (det(t(x)%*%x) != 0){yhat = x%*%solve(t(x)%*%x)%*%t(x)%*%d$y
    sses_3[counter,] = c(t(d$y-yhat)%*%(d$y-yhat), i, j)}
  }
}



opk_3 = which(sses_3[,1] == min(sses_3[,1], na.rm=T))
sses_3[opk_3,]
m.sses[4] = sses_3[opk_3, 1]

m.sses

plot(1:4, m.sses, type = "l",
     main = "Elbow plot of SSE drops per MARS step",
     xlab = "Number of MARS steps (1 - 4)",
     ylab = "SSE",
     col = "blue")


#Final optimal MARS regression model

x1 = d$x1
x2 = d$x2
x2.1 = ifelse(x2 < x2[sses[opk,3]], x2, 0)
x2.2 = ifelse(x2 > x2[sses[opk,3]], x2, 0)
x3.1 = ifelse(x2 < x2[sses_2[opk_2,3]], x2, 0)
x3.2 = ifelse(x2 > x2[sses_2[opk_2,3]], x2, 0)
x4.1 = (x2.1 * ifelse(x2 < x2[sses[opk_3,3]], x2, 0)) 
x4.2 = (x2.1 * ifelse(x2 > x2[sses[opk_3,3]], x2, 0))
x = cbind(intercept, x2.1, x2.2, x3.1, x3.2, x4.1, x4.2)

if (det(t(x)%*%x) != 0){yhat = x%*%solve(t(x)%*%x)%*%t(x)%*%d$y}

betas = solve(t(x)%*%x)%*%t(x)%*%d$y

final_ess = t((d$y - yhat))%*%(d$y - yhat)

r2 = 1- final_ess/tss



#Final plot of model
dd = rbind(d, d)
x1s = dd[,1]
x2s = dd[,2]
ys = Reduce(c,list(d$y, yhat))
class = Reduce(c, list(rep("Observed", 20), rep("Predicted", 20)))
colors <- c("blue", "red")
colors <- colors[as.numeric(factor(class))]

model_fit_plot = scatterplot3d(x1s, x2s, ys, pch = 16, color=colors, type = "h",
                               main = "Comparison of observerd and MARS predicted values",
                               xlab = "X1",
                               ylab = "X2",
                               zlab = "Y")
legend("left", legend = unique(class),
       col =  c("blue", "red"), pch = 16, inset = 0.1)
q1plot$plane3d(data.lm) #what would have been fitter by multiple linear regression.





#Make a uniform grid over x1 and x2, and evaluate the final regression function on this grid
gs = 40
x1g = seq(min(d$x1), max(d$x1), length.out = gs)
x2g = seq(min(d$x2), max(d$x2), length.out = gs)

xpairs = matrix(NA, nrow = gs^2, ncol = 2)
counter = 0
for (i in 1:length(x1g)){
  for (j in 1:length(x2g)){
    counter = counter + 1
    x1g_i = x1g[i]
    x2g_j = x2g[j]
    xpairs[counter,] = c(x1g_i, x2g_j)
  } 
}


yhats = matrix(NA, nrow = gs^2)
counter = 0
for (i in 1:length(x1g)){
  for (j in 1:length(x2g)){
    x2g_j = x2g[j]
    counter = counter+1
    yhats[counter] = betas[1] + betas[2]*ifelse(x2g_j < d$x2[sses[opk,3]], x2g_j, 0) +
                                betas[3]*ifelse(x2g_j > d$x2[sses[opk,3]], x2g_j, 0) +
                                betas[4]*ifelse(x2g_j < d$x2[sses_2[opk_2,3]], x2g_j, 0) +
                                betas[5]*ifelse(x2g_j > d$x2[sses_2[opk_2,3]], x2g_j, 0) +
                                betas[6]*(ifelse(x2g_j < d$x2[sses[opk,3]], x2g_j, 0) * ifelse(x2g_j < d$x2[sses[opk_3,3]], x2g_j, 0)) +
                                betas[7]*(ifelse(x2g_j < d$x2[sses[opk,3]], x2g_j, 0) * ifelse(x2g_j > d$x2[sses[opk_3,3]], x2g_j, 0))
    
    } 
}







library(rgl)
# Initialize the scene, no data plotted

coords = cbind(xpairs, yhats)
open3d()
plot3d(d$x1, d$x2, d$y, type = "s", col = "red", size = 1) 
rglwidget()
plot3d(xpairs[,1], xpairs[,2], yhats, type = "s", col = "red", size = 1)
rglwidget()
# Add our plain
planes3d(coords, type = "s", col = "red", size = 1)
#planes3d(betas[2:7], betas[2:7], -1, betas[1], type = "s", col = "red")
?planes3d










