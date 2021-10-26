library(imager)    # image loading and processing
library(dplyr)     # data manipulation
library(ggplot2)   # data visualization
library(tidyr)     # data wrangling
library(magick)
library(purrr)
library(purrrlyr)
library(data.table)
library(class)
library(stringr)
library(caret)
library(beepr)
library(stringr)
library(qcc)
library(colordistance)
library(keras)

#TRAINING DATA

setwd("c:/Users/dbaue/Desktop/APP_NEW/Data/Train_new/")

allFiles = list.files(path = ".", pattern = ".jpg")
allInfo = image_info(image_read(allFiles))
allInfo$fileName = allFiles

n <- nrow(allInfo)
target_size_k <- 50

train_matrix <- list()

start_time <- Sys.time()
for (i in 1:length(allFiles)){
  
  img <- load.image(allFiles[i])
  img <- imager::resize(im = img, size_x = target_size_k, size_y = target_size_k)
  image_df <- as.data.frame(img)
  
  red <- image_df[which(image_df$cc ==1),]
  red_mat <- matrix(data = red$value, nrow = target_size_k, ncol = target_size_k, byrow = TRUE)
  blue <- image_df[which(image_df$cc == 2),]
  blue_mat <- matrix(data = blue$value, nrow = target_size_k, ncol = target_size_k, byrow = TRUE)
  green <- image_df[which(image_df$cc == 3),]
  green_mat <- matrix(data = green$value, nrow = target_size_k, ncol = target_size_k, byrow = TRUE)
  
  matrix <- (red_mat+blue_mat+green_mat)/3
  
  label_s <- allInfo$fileName[i]
  lab <- rep(x = label_s,target_size_k)
  
  image_1 <- cbind(lab,matrix)
  im_df <- data.frame(label = lab, mat = matrix)
  
  train_matrix[[i]] <- im_df
  
}
end_time <- Sys.time()
training_time <- start_time-end_time


data_train <- rbindlist(train_matrix, fill = TRUE)
train_x <- select(data_train, c(2:51))
label_train <- select(data_train, 1)
label_train <- label_train$label

setwd(dir = "c:/Users/dbaue/Desktop/APP_NEW/")
save(train_x, file = "train_x")
save(label_train, file = "label_train")
save(train_matrix, file = "train_matrix")

load("train_x")

#DO PRINCIPAL COMPONENT ANALYSIS
train_x.pc <- prcomp(x = train_x, center = TRUE, scale. = TRUE)
summary(train_x.pc)

train_x_new <- train_x[,1:25]
save(train_x_new, file = "train_x_new")


#TESTING DATA

setwd("c:/Users/dbaue/Desktop/APP_NEW/Data/Test_1//")

allFiles = list.files(path = ".", pattern = ".jpg")
allInfo = image_info(image_read(allFiles))
allInfo$fileName = allFiles

n <- nrow(allInfo)

test_matrix <- list()

for (i in 1:length(allFiles)){
  
  perc <- i/length(allFiles)*100
  print(perc)
  img <- load.image(allFiles[i])
  image_df <- as.data.frame(img)
  red <- image_df[which(image_df$cc ==1),]
  red_mat <- matrix(data = red$value, nrow = 50, ncol = 50, byrow = TRUE)
  blue <- image_df[which(image_df$cc == 2),]
  blue_mat <- matrix(data = blue$value, nrow = 50, ncol = 50, byrow = TRUE)
  green <- image_df[which(image_df$cc == 3),]
  green_mat <- matrix(data = green$value, nrow = 50, ncol = 50, byrow = TRUE)
  
  matrix <- (red_mat+blue_mat+green_mat)/3
  
  label_s <- allInfo$fileName[i]
  lab <- rep(x = label_s,50)
  image_1 <- cbind(lab,matrix)
  im_df <- data.frame(label = lab, mat = matrix)
  
  test_matrix[[i]] <- im_df
  
}

data_test <- rbindlist(test_matrix, fill = TRUE)
testx <- select(data_test, c(2:51))
label_test <- select(data_test, 1)

setwd(dir = "c:/Users/dbaue/Desktop/APP_NEW/")
save(testx, file = "testx")
save(label_train, file = "label_test")
save(test_matrix, file = "test_matrix")

load("testx")
#PRINCIPAL COMPONENT ANALYSIS

testx.pca <- prcomp(x = testx, center = TRUE, scale. = TRUE)
summary(testx.pca)
test_x_new <- testx[,1:25]

#MODEL FIT
model <- knn1(train = train_x_new, test = test_x_new, cl = label_train)

cm <- table(label_test$label, model)
misClassError <- mean(model != label_test$label)
print(paste('Accuracy =', 1-misClassError))

#PREDICT ONE IMAGE
save(predict_one_image, file = "predict_one_image")
path <- "c:/Users/dbaue/Desktop/APP/data/Test/Apple Braeburn/Apple_Braeburn (1).jpg"
target_size <- c(50,50,3)

im <- image_load(path = path, target_size = target_size[1:2])
im_df <- image_to_array(im)
im_df <- im_df/255
red <- im_df[, , 1]
blue <- im_df[, , 2]
green <- im_df[, , 3]
matrix <- (red+ green+ blue)/3
testx_1 <- as.data.table(matrix)
testx_1.pca <- prcomp(x = testx_1, center = TRUE, scale. = TRUE)
summary(testx_1.pca)
testx_1_new <- testx_1[,1:25]

model1 <- knn1(train = train_x_new, test = testx_1_new, cl = label_train)
cm_1 <- table(label_test_1, model1)
cm_1 <- as.data.frame(cm_1)
new_cm <- cm_1[order(cm_1$Freq, decreasing = TRUE),][1:5,]
fruit_knn <- new_cm$model1

class <- str_remove(string = fruit_knn,pattern = "\\s*\\([^\\)]+\\).jpg")
class_1 <- str_replace_all(string = class, pattern = "_", replacement = " ")
dat <- data.frame("Fruit" = class)
dat

#Bootstrap procedure


load("train_x")
load("label_train")
load("train_matrix")

fruit_list <- c("Apple_Braeburn", "Apple_crimson", "Apple_golden", "Apple_golden_2 ", "Apple_golden_3",
                "Apple_granny_smith", "Apple_pink_lady", "Apple_red_1", "Apple_red_2", "Apple_red_3",
                "Apple_red_delicious", "Apple_red_yellow_1", "Apple_red_yellow_2", "Apricot", "Avocado",
                "Avocado_ripe", "Banana", "Banana_lady_finger", "Banana_red","Beetroot", "Blueberry", "cactus_fruit",
                "Cantaloupe_1", "Canteloupe_2", "Carambula", "Cauliflower", "Cherry_1", "Cherry_2", "Cherry_rainier", 
                "Cherry_wax_black", "Cherry_wax_red", "Cherry_wax_yellow", "Chestnut", "Clementine", "Cocos", "Corn",
                "Corn_husk", "Cucumber_ripe", "Cucumber_ripe_1", "Dates", "Eggplant", "Fig", "Ginger_root", "Granadilla",
                "Grape_blue", "Grape_pink", "Grape_white", "Grape_white_2", "Grape_white_3", "Grape_white_4",
                "Grapefruit_pink", "Grapefruit_white", "Guava", "Hazelnut", "Huckleberry", "Kaki", "Kiwi", "Kohlrabi",
                "Kumquats", "Lemon", "Lemon_Meyer", "Limes", "Lychee","Mandarine","Mango", "Mango_Red", "Mangostan", "Maracuja",
                "Melon_Piel_de_Sapo", "Mulberry", "Nectarine", "Nectarine_Flat", "Nut_Forest", "Nut_Pecan", "Onion_Red", 
                "Onion_Red_Peeled", "Onion_White", "Orange", "Papaya", "Passion_Fruit", "Peach", "Peach_2", "Peach_Flat", 
                "Pear", "Pear_2", "Pear_Abate", "Pear_Forelle", "Pear_Kaiser", "Pear_Monster", "Pear_Red", "Pear_Stone",
                "Pear_Williams", "Pepino", "Pepper_Green", "Pepper_Orange", "Pepper_Red", "Pepper_Yellow", "Physalis",
                "Physalis_Husk", "Pineapple", "Pineapple_Mini", "Pitahaya_Red", "Plum", "Plum_2", "Plum_3", 
                "Pomegranate", "Pomelo_Sweetie", "Potato_Red", "Potato_Red_Washed", "Potato_Sweet", "Potato_White",
                "Quince", "Rambutan", "Rasberry", "Redcurrant", "Salak", "Strawberry", "Strawberry_Wedge", 
                "Tamarillo", "Tangelo", "Tomato_1", "Tomato_2", "Tomato_3", "Tomato_4", "Tomato_Cherry_Red", 
                "Tomato_Heart", "Tomato_Maroon", "Tomato_not_Ripened", "Tomato_Yellow", "Walnut", "Waterlemon")
n_fruit <- length(fruit_list)

fruit_per_class_train <- table(factor(train_image_array_gen$classes))
image_per_class_train <- as.vector(fruit_per_class_train)

fruit_per_class_test <- table(factor(valid_image_array_gen$classes))
image_per_class_test <- as.vector(fruit_per_class_test)

obs_index_train <- cumsum(image_per_class_train)
obs_index_train <- append(obs_index_train,1,0)

obs_index_test <- cumsum(image_per_class_test)
obs_index_test <- append(obs_index_test,1,0)

accuracy_boot <- list()
start_time <- Sys.time()
for (iter in 1:100){

training_sample <- list()

for (i in 1:n_fruit) {
  
  if (i == 1){
    fruit_i <- list()
    for (j in obs_index_train[i]:obs_index_train[i+1]) {
      one_image <- train_matrix[[j]]
      fruit_i[[j]] <- one_image
      
    }
  }else{
    start_index <-  obs_index_train[i]+1
    end_index <- obs_index_train[i+1]
    fruit_i <- list()
    for (j in start_index:end_index) {
      k <- j-obs_index_train[i]
      one_image <- train_matrix[[j]]
      fruit_i[[k]] <- one_image
    }
  }
  samp_size <- 50
  n <- length(fruit_i)
  ind <- seq(1,n,1)
  samp <- sample(x = ind,size = samp_size, replace = FALSE)
  train_fruit <- fruit_i[samp]
  
  training_sample[[i]] <- train_fruit
}


testing_sample <- list()
for (i in 1:n_fruit) {

  fruit_i <- training_sample[[i]]
  index_i <- round(runif(1,1,50))
  fruit_new <- fruit_i[[index_i]]
  test_fruit <- fruit_new
  
  testing_sample[[i]] <- test_fruit
}

training_sample_new <- list()
for (i in 1:length(training_sample)) {
  tr <- training_sample[[i]]
  train_sample <- rbindlist(tr, fill = TRUE)
  training_sample_new[[i]] <- train_sample
  
}
testing_sample_new <- testing_sample
model_train_sample <- rbindlist(training_sample_new, fill = TRUE)
model_train_sample <- as.matrix(model_train_sample)
model_train_matrix <- model_train_sample[,2:51]
model_train_label <- model_train_sample[,1]
model_test_sample <- rbindlist(testing_sample_new, fill = TRUE)
model_test_sample <- as.matrix(model_test_sample)
model_test_label <- model_test_sample[,1]
model_test_matrix <- model_test_sample[,2:51]

model_sample <- knn1(train = model_train_matrix, test = model_test_matrix, cl = model_train_label)

confusion_matrix <- table(model_test_label, model_sample)
cm <- as.data.frame(confusion_matrix)

mis_Classification <- mean(model_sample != model_test_label)
Accuracy <- 1-mis_Classification
accuracy_boot[[iter]] <- Accuracy
perc <- iter/10*100
print(paste(perc,"%"))
}

accuracy_bootstrap <- flatten_dbl(accuracy_boot)
hist(accuracy_bootstrap, breaks = 11, freq = FALSE, xlab = "Accuracy", main = "")
