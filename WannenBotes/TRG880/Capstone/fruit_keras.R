#This code was sourced from work done by For-loops and piep kicks:
# https://forloopsandpiepkicks.wordpress.com/2021/03/16/how-to-build-your-own-image-recognition-app-with-r-part-1/

library(reticulate)
library(keras)
library(tensorflow)
library(tidyverse)

wd_linux = "/home/rian/Documents/Fruit360/Fruit-Images-Dataset-master/Training/"

curr_wd = wd_linux
setwd(curr_wd)

fruit_names = list.files(curr_wd)
output_n = length(fruit_names)
#save(fruit_names, file="fruit_names.RData")

width = 100
height = 100
target_size = c(width, height)
rgb = 3 #color channels


train_data_gen <- image_data_generator(rescale = 1/255, 
                                       validation_split = .2)


train_images <- flow_images_from_directory(curr_wd,
                                           train_data_gen,
                                           subset = 'training',
                                           target_size = target_size,
                                           class_mode = "categorical",
                                           shuffle=F,
                                           classes = fruit_names,
                                           seed = 2021)
validation_images <- flow_images_from_directory(curr_wd,
                                                train_data_gen, 
                                                subset = 'validation',
                                                target_size = target_size,
                                                class_mode = "categorical",
                                                classes = fruit_names,
                                                seed = 2021)
table(train_images$classes)

plot(as.raster(train_images[[1]][[1]][18,,,]))

#Training a convoluted neural network on our data (using the xception pre-built model):

mod_base <- application_xception(weights = 'imagenet', 
                                 include_top = FALSE, input_shape = c(width, height, 3))

freeze_weights(mod_base) 


#Now let’s write a small function that builds a layer on top of the pre-trained network 
#and sets a few parameters to variabes that we can later use to tune the model:

model_function <- function(learning_rate = 0.001, 
                           dropoutrate=0.2, n_dense=1024){
  
  k_clear_session()
  
  model <- keras_model_sequential() %>%
    mod_base %>% 
    layer_global_average_pooling_2d() %>% 
    layer_dense(units = n_dense) %>%
    layer_activation("relu") %>%
    layer_dropout(dropoutrate) %>%
    layer_dense(units=output_n, activation="softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learning_rate),
    metrics = "accuracy"
  )
  
  return(model)
  
}

model <- model_function()
model

batch_size <- 32
epochs <- 2

hist <- model %>% fit(
  train_images,
  steps_per_epoch = train_images$n %/% batch_size, 
  epochs = epochs, 
  validation_data = validation_images,
  validation_steps = validation_images$n %/% batch_size,
  verbose = 1
)

test_data_gen <- image_data_generator(rescale = 1/255)

path_test = "/home/rian/Documents/Fruit360/Fruit-Images-Dataset-master/Test/"


test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = fruit_names,
                                          shuffle = F,
                                          seed = 2021)

model %>% evaluate(test_images, 
                             steps = test_images$n)

model %>% save_model_tf("fruity")
#model %>% save_model_hdf5("Xception_cnn_h5")


#REMEMBER, THE NAME OF THE MODEL MUST BE FRUITY!