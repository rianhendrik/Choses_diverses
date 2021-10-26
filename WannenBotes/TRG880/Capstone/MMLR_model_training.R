#Training the multimomial logistic regression model.
library(nnet)
#Setting the reference class:
d = readRDS("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/Design Matrices/mmlr_train_all_size50")
d$fruit <- as.factor(d$fruit)
d$fruit <- relevel(d$fruit, ref = "Apple Braeburn")

start = Sys.time()
multinom_model <- multinom(fruit ~ ., data = d, maxit=700,MaxNWts=84581)
end = Sys.time()
duration = end - start
duration
saveRDS(multinom_model,
        "/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/rgbquads_logit_all_size50.rds")
# Checking the model
#Training the multimomial logistic regression model.

#Setting the reference class:
mod = readRDS("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/rgbquads_logit_all_size50.rds")
summary(mod)$coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


d$fruit <- as.factor(d$fruit)
d$fruit <- relevel(d$fruit, ref = "Apple Braeburn")

start = Sys.time()
multinom_model <- multinom(fruit ~ ., data = d, maxit=700,MaxNWts=84581)
end = Sys.time()
duration = end - start
duration
saveRDS(multinom_model,
        "/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/rgbquads_logit_all_size50.rds")
# Checking the model
loadRDS("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/rgbquads_logit_all_size50.rds")
summary(multinom_model)
res =multinom_model$fitted.values
#write_xlsx(data.frame(res),
#      paste0("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/logit_res_rgb_quads.xlsx"))


train_predicted = predict(multinom_model, newdata = d, "class")
tab <- table(d$fruit, train_predicted)
round((sum(diag(tab))/sum(tab))*100,2)
#100% accuracy on training set!
summary(multinom_model)
res =multinom_model$fitted.values
#write_xlsx(data.frame(res),
#      paste0("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/logit_res_rgb_quads.xlsx"))


train_predicted = predict(multinom_model, newdata = d, "class")
tab <- table(d$fruit, train_predicted)
round((sum(diag(tab))/sum(tab))*100,2)
#100% accuracy on training set!