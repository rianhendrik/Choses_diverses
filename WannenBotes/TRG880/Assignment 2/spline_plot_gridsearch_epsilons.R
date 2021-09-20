#Spline plots
#setwd("/home/rian/Dropbox/2. TRG880/1. Assignments/Assignment 2")
setwd("C:/Users/rianh/Dropbox/2. TRG880/1. Assignments/Assignment 2")
d = openxlsx::read.xlsx("ex1.xlsx")
yhat = unlist(openxlsx::read.xlsx("gridsearch_yhat.xlsx"))
x = d$x
y = d&y


e1 = 389.628 #grid search epsilon 1
e2 = 479.628 #grid search epsilon 2

theta1 = -15561.57
theta2 = 160.97283
theta3 = -0.524031
theta4 = 0.0005495
theta5 = 0.0011692
theta6 = -0.001398

xi = e1

yhati = theta1*1 + theta2*xi + theta3*xi^2 + theta4*xi^3 + theta5*(xi-e1)^3 + theta6*(xi-e2)^3

?plot()


plot(d$x, d$y, pch = 19,
     main = "Cubic polynomial fitted to some data",
     xlab = "X",
     ylab = "Yhat")
lines(x, yhat, col = "blue")
points(c(e1,e2), c(0,0), col = c("blue", "red"), pch = 25)
legend("top", legend=c("389.628", "479.628"),
       col=c("blue", "red"), pch=c(25, 25))
