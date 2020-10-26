
##########################################################################################################
#READ ME!
#This code is based on the work done my H. Zhu. His code and documentation may be access on the link below:
#https://rpubs.com/H_Zhu/246450
############################################################################################################


#Importing the data

x = read.table( "C:\\Users\\rianh\\OneDrive - University of Pretoria\\Documents\\Rian 2020\\Semester 2\\EKT 725\\Assignment 5\\Data\\m3.txt", fill=TRUE)

x = as.numeric(as.character(unlist(x)))

#Plotting the kernel density
plot(density(x))

#Using k-means to find reasonable initial parameter estimates

three_clusters = kmeans(x, 3)$cluster
mu1 = mean(x[three_clusters == 1])
mu2 = mean(x[three_clusters == 2])
mu3 = mean(x[three_clusters == 3])
sigma1 = sd(x[three_clusters == 1])
sigma2 = sd(x[three_clusters == 2])
sigma3 = sd(x[three_clusters == 3])
pi1 = sum(three_clusters == 1)/length(three_clusters)
pi2 = sum(three_clusters == 2)/length(three_clusters)
pi3 = sum(three_clusters == 3)/length(three_clusters)


Q = 0

#This is the starting value of the complete log likelihood function, with gamma_i = pi_i

Q[2] = sum(log(pi1) + log(dnorm(x, mu1, sigma1))) + sum(log(pi2) + log(dnorm(x, mu2, sigma_2))) + sum(log(pi3) + log(dnorm(x, mu3, sigma_3)))

k = 2

while (abs(Q[k] - Q[k - 1]) >= 1e-60) {
  # E step
  
  comp1 = pi1*dnorm(x, mu1,sigma1)
  comp2 = pi2*dnorm(x, mu2, sigma2)
  comp3 = pi3*dnorm(x, mu3, sigma3)
  comp.sum = comp1 + comp2 + comp3
  
  gamma1 = comp1/comp.sum
  gamma2 = comp2/comp.sum
  gamma3 = comp3/comp.sum
  
  #M Step
  pi1 = sum(gamma1)/length(x)
  pi2 = sum(gamma2)/length(x)
  pi3 = sum(gamma3)/length(x)
  
  mu1 = sum(gamma1*x)/sum(gamma1)
  mu2 = sum(gamma2*x)/sum(gamma2)
  mu3 = sum(gamma3*x)/sum(gamma3)
  
  sigma1 = sqrt(sum(gamma1*(x - mu1)**2)/sum(gamma1))
  sigma2 = sqrt(sum(gamma2*(x - mu2)**2)/sum(gamma2))
  sigma3 = sqrt(sum(gamma3*(x - mu3)**2)/sum(gamma3))
  
  k = k + 1
  Q[k] = sum(log(comp.sum))
  
}

mu1
mu2
mu3
sigma1
sigma2
sigma3

