#Test set:
library(nnet)
options(scipen=999)
multinom_model = readRDS("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/rgbquads_logit_all_size50.rds")
d_test = readRDS("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/Design Matrices/mmlr_test_all_size50")
d_train = readRDS("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/Design Matrices/mmlr_train_all_size50")
pred = predict(multinom_model, newdata = d_test[500,], "class", type = "prob")

z <- summary(multinom)$coefficients/summary(test)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

pred_mmlr = round(data.frame(pred),8)
pred_mmlr = head(pred_mmlr[order(-pred_mmlr$pred),, drop = F])
pred_mmlr = data.frame(Fruit = rownames(pred_mmlr), Prediction = pred_mmlr$pred)
pred_mmlr$Prediction = sprintf("%.2f %%", 100*pred_mmlr$Prediction)


test_predicted = predict(multinom_model, newdata = d_test, "class")
tab <- table(d_test$fruit, test_predicted)
round((sum(diag(tab))/sum(tab))*100,2)
tab

