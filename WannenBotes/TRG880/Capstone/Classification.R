library(reticulate)
library(tensorflow)
library(keras)
library(kerasR)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(tensorflow)
library(ggplot2)
library(imager)

#install.packages("keras")

#image_load(path = "c:/Users/dbaue/Desktop/APP_NEW/Data/Test/Apple Braeburn/Apple_Braeburn (1).jpg")

fruit_list <- c("Apple Braeburn", "Apple Crimson Snow", "Apple Golden 1", "Apple Golden 2", "Apple Golden 3",
                "Apple Granny Smith", "Apple Pink Lady", "Apple Red 1", "Apple Red 2", "Apple Red 3",
                "Apple Red Delicious", "Apple Red Yellow 1", "Apple Red Yellow 2", "Apricot", "Avocado",
                "Avocado Ripe", "Banana", "Banana Lady Finger", "Banana Red","Beetroot", "Blueberry", "Cactus fruit",
                "Cantaloupe 1", "Cantaloupe 2", "Carambula", "Cauliflower", "Cherry 1", "Cherry 2", "Cherry Rainier", 
                "Cherry Wax Black", "Cherry Wax Red", "Cherry Wax Yellow", "Chestnut", "Clementine", "Cocos", "Corn",
                "Corn Husk", "Cucumber Ripe", "Cucumber Ripe 2", "Dates", "Eggplant", "Fig", "Ginger Root", "Granadilla",
                "Grape Blue", "Grape Pink", "Grape White", "Grape White 2", "Grape White 3", "Grape White 4",
                "Grapefruit Pink", "Grapefruit White", "Guava", "Hazelnut", "Huckleberry", "Kaki", "Kiwi", "Kohlrabi",
                "Kumquats", "Lemon", "Lemon Meyer", "Limes", "Lychee","Mandarine","Mango", "Mango Red", "Mangostan", "Maracuja",
                "Melon Piel de Sapo", "Mulberry", "Nectarine", "Nectarine Flat", "Nut Forest", "Nut Pecan", "Onion Red", 
                "Onion Red Peeled", "Onion White", "Orange", "Papaya", "Passion Fruit", "Peach", "Peach 2", "Peach Flat", 
                "Pear", "Pear 2", "Pear Abate", "Pear Forelle", "Pear Kaiser", "Pear Monster", "Pear Red", "Pear Stone",
                "Pear Williams", "Pepino", "Pepper Green", "Pepper Orange", "Pepper Red", "Pepper Yellow", "Physalis",
                "Physalis with Husk", "Pineapple", "Pineapple Mini", "Pitahaya Red", "Plum", "Plum 2", "Plum 3", 
                "Pomegranate", "Pomelo Sweetie", "Potato Red", "Potato Red Washed", "Potato Sweet", "Potato White",
                "Quince", "Rambutan", "Raspberry", "Redcurrant", "Salak", "Strawberry", "Strawberry Wedge", 
                "Tamarillo", "Tangelo", "Tomato 1", "Tomato 2", "Tomato 3", "Tomato 4", "Tomato Cherry Red", 
                "Tomato Heart", "Tomato Maroon", "Tomato not Ripened", "Tomato Yellow", "Walnut", "Watermelon")


n_fruit <- length(fruit_list)

img_width <- 50
img_height <- 50
target_size <- c(img_width, img_height)
channels <- 3

train_path <- "c:/Users/dbaue/Desktop/APP_NEW/Data/Training/"
test_path <- "c:/Users/dbaue/Desktop/APP_NEW/Data/Test/"

training_data <- image_data_generator(rescale = 1/255,
                                      rotation_range = 40,
                                      width_shift_range = 0.2,
                                      height_shift_range = 0.2,
                                      shear_range = 0.2,
                                      zoom_range = 0.2,
                                      horizontal_flip = TRUE,
                                      fill_mode = "nearest")

testing_data_gen <- image_data_generator(
  rescale = 1/255
) 

train_image_array_gen <- flow_images_from_directory(train_path, 
                                                    training_data,
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = fruit_list,
                                                    seed = 42,subset = "training")


valid_image_array_gen <- flow_images_from_directory(directory = test_path, 
                                                    testing_data_gen,
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = fruit_list,
                                                    seed = 42)


#PLOTTING NUMBER OF IMAGES PER CLASS

cat("Number of images per class:")
classes <- table(factor(train_image_array_gen$classes))
classes <- as.vector(classes)

cat("\nClass label vs index mapping:\n")

classes_df <- data.frame("Fruit" = fruit_list, "Classes" = classes)
classes_df <- classes_df[order(classes_df$Classes),]

class_1 <- classes_df[1:44,]

ggp_1 <- ggplot(class_1, aes(x = Fruit, y = Classes)) +
  geom_bar(stat = "identity") +
  ylab("Number of images per class")

ggp_1 +  coord_flip()

class_2 <- classes_df[45:90,]

ggp_2 <- ggplot(class_2, aes(x = Fruit, y = Classes)) +
  geom_bar(stat = "identity") +
  ylab("Number of images per class")

ggp_2 +  coord_flip()

class_3 <- classes_df[91:131,]

ggp_3 <- ggplot(class_3, aes(x = Fruit, y = Classes)) +
  geom_bar(stat = "identity") +
  ylab("Number of images per class")

ggp_3 +  coord_flip()

mean_image_per_class <- mean(classes_df$Classes)
median_image_per_class <- median(classes_df$Classes)

#PLOTTING HISTOGRAMS FOR RGB VALUES

image_new <- load.image(file = "c:/Users/dbaue/Desktop/APP/data/Test/Blueberry/186_100.jpg")
image_df <- as.data.frame(image_new)

image_df <- subset(x = image_df, subset = image_df$value < 0.8)

bdf <- mutate(image_df,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

ggplot(bdf, aes(value,col=channel)) +
  geom_histogram(data = subset(bdf, channel == "R"), fill = "red", alpha = 0.9)+
  geom_histogram(data = subset(bdf, channel == "G"), fill = "green", alpha = 0.9)+
  geom_histogram(data = subset(bdf, channel == "B"), fill = "blue", alpha = 0.7)

#SAVE FRUIT CLASSES AND INDICES

fruits_classes_indices <- train_image_array_gen$class_indices
save(fruits_classes_indices, file = "/Users/dbaue/Desktop/MASTERS 2021/SEMESTER 2/TRG 880/CAPSTONE PROJECT/DATA/data/fruits_classes_indices")

# number of training samples
train_samples <- train_image_array_gen$n
# number of testing samples
test_samples <- valid_image_array_gen$n
# define batch size and number of epochs
batch_size <- 32
epochs <- 20



# initialise model
model <- keras_model_sequential()

# add layers
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels), strides = 1) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2), padding = "same") %>%
  
  # Second hidden layer
  layer_conv_2d(filter = 64, kernel_size = c(2,2), padding = "same", strides = 1) %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
    
  #Third hidden layer
  layer_conv_2d(filters = 128, kernel_size = c(3,3), strides = 1, padding = "same") %>%
  layer_max_pooling_2d(pool_size = c(2,2), padding = "same") %>%
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  # Outputs from dense layer are projected onto output layer
  layer_dense(n_fruit) %>% 
  layer_activation("softmax")


# compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(learning_rate =  0.001, decay = 1e-6),
  metrics = "accuracy"
)

# fit
hist <- model %>% fit(
  # training data
  train_image_array_gen,
  shuffle = TRUE,
  # epochs
  steps_per_epoch = as.integer(train_samples / batch_size), 
  epochs = epochs, 
  
  # validation data
  validation_data = valid_image_array_gen,
  validation_steps = as.integer(test_samples / batch_size),
  
  # print progress
  verbose = 2,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint("/Users/dbaue/Desktop/MASTERS 2021/SEMESTER 2/TRG 880/CAPSTONE PROJECT/CODING/fruits_checkpoints.h5", save_best_only = TRUE),
    # only needed for visualising with TensorBoard
    callback_tensorboard(log_dir = "/Users/dbaue/Desktop/MASTERS 2021/SEMESTER 2/TRG 880/CAPSTONE PROJECT/CODING/logs")
  )
) 

model_1 <- model %>% model_function()

setwd(dir = "c:/Users/dbaue/Desktop/APP_NEW/model/")
model %>% save_model_tf("model_new", include_optimizer = TRUE)

save_model_tf(object = model,"my_model.h5")

model <- load_model_tf("model_new/")
model_1 <- model %>% model_function()


classes <- valid_image_array_gen$classes %>%
  factor() %>%
  table() %>%
  as_tibble()
colnames(classes)[1] <- "value"

indices <- valid_image_array_gen$class_indices %>%
  as.data.frame() %>%
  gather() %>%
  mutate(value = as.character(value)) %>%
  left_join(classes, by = "value")

valid_image_array_gen$reset()

valid_image_array_gen

predictions <- model %>%
  predict(valid_image_array_gen) %>%
  round(digits = 2) %>%
  as_tibble()

colnames(predictions) <- fruit_list

predictions <- predictions %>%
  mutate(truth_loc = as.character(valid_image_array_gen$classes)) %>%
  left_join(indices, by = c("truth_loc" = "value"))

pred_analysis <- predictions %>%
  mutate(img_id = seq(1:valid_image_array_gen$n)) %>%
  gather(pred_lbl, y, Kiwi:Pomegranate) %>%
  group_by(img_id) %>%
  filter(y == max(y)) %>%
  arrange(img_id) %>%
  group_by(key,n,pred_lbl) %>%
  count(name = "nn")


p <- pred_analysis %>%
  mutate(percentage_pred = nn / n * 100) %>%
  ggplot(aes(x = key, y = pred_lbl, 
             fill = percentage_pred,
             label = round(percentage_pred, 2))) +
  geom_tile() +
  scale_fill_continuous() +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_text(color = "white") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "True class", 
       y = "Predicted class",
       fill = "Percentage\nof predictions",
       title = "True v. predicted class labels", 
       subtitle = "Percentage of test images predicted for each label",
       caption = "For every class of test images, this figure shows the percentage of images with predicted labels for each possible label.
       E.g.: 100% of test images in the class 'Apricot' were predicted correctly. Of test images from the class 'Cocos' 
       only 94.58% were predicted correctly, while 0.6% of these images were predicted to show a Strawberry and 4.82% a Pineapple.")

p2 <- pred_analysis %>%
  mutate(prediction = case_when(key == pred_lbl ~ "correct",TRUE ~ "false")) %>%
  group_by(key, prediction, n) %>%
  summarise(sum = sum(nn)) %>%
  mutate(percentage_pred = sum/n *100)


p2 <- pred_analysis %>%
  mutate(prediction = case_when(
    key == pred_lbl ~ "correct",
    TRUE ~ "false"
  )) %>%
  group_by(key, prediction, n) %>%
  summarise(sum = sum(nn)) %>%
  mutate(percentage_pred = sum / n * 100) %>%
  ggplot(aes(x = key, y = prediction, 
             fill = percentage_pred,
             label = round(percentage_pred, 2))) +
  geom_tile() +
  scale_fill_continuous() +
  geom_text(color = "white") +
  coord_flip() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "True class", 
       y = "Prediction is...",
       fill = "Percentage\nof predictions",
       title = "Percentage of correct v false predictions", 
       subtitle = "Percentage of test image classes predicted correctly v. falsely",
       caption = "For every class of test images, this figure shows the percentage of 
       images with correctly and falsely predicted labels. E.g.: 100% of test images 
       in the class 'Apricot' were predicted correctly. Of test images from the class 
       'Cocos' only 94.58% were predicted correctly, while 5.42% were predicted falsely.")
p2

##predict one image

fruit_class <- function(model, test_image_path, target_size){
  
  test_image_one <- image_load(path = test_image_path, target_size = target_size)
  
  x <- image_to_array(test_image_one)
  x <- array_reshape(x, c(1,dim(x)))
  x <- x/255
  
  pred_x <- model %>% predict(x)
  
  
  pred_x <- data.frame("Item" = fruit_list, "Probability" = t(pred_x))
  
  index <- which.max(pred_x$Probability)
  fruit <- pred_x[index,]
  fruit_type <- fruit$Item
  print("The fruit is classified as:")
  print(fruit_type)
}

img_width <- 20
img_height <- 20
target_size <- c(img_width, img_height)
test_path <- "c:/Users/dbaue/Desktop/MASTERS 2021/SEMESTER 2/TRG 880/CAPSTONE PROJECT/DATA/data/Test/Mandarine/149_100.jpg"


orange <- fruit_class(model = model, test_image_path = test_path, target_size = target_size)











