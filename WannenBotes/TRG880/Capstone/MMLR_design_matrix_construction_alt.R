library(nnet)
library(EBImage)
library(writexl)
library(keras)
library(tensorflow)

linux_fp_train = "/home/rian/Documents/Fruit360/Fruit-Images-Dataset-master/Training"
linux_fp_test = "/home/rian/Documents/Fruit360/Fruit-Images-Dataset-master/Test"

setwd(linux_fp_test)

size = 50
ts <- c(size, size,3)


n = size
pixels = n*n
r_indx = 1:pixels
g_indx = pixels+1:pixels*2
b_indx = (2*pixels+1):(pixels*3)

fruit_names = list.files()
labels = fruit_names
num_imgs = 0
for (pic in labels){
  num_imgs = num_imgs + length(list.files(paste0(pic)))
  #num_imgs = num_imgs + length(list.files(paste0(pic), pattern = "^r_"))
  #num_imgs = num_imgs + length(list.files(paste0(pic), pattern = "^[0-9]"))
}


#Building the desing matrix

d1 = data.frame(matrix(NaN, num_imgs, 13))
names(d1) = c("fruit", "r1", "r2", "r3", "r4",
              "g1", "g2", "g3", "g4",
              "b1", "b2", "b3", "b4")

col_pp = 4 #Columns per pixel

counter = 0
start = Sys.time()
for (i in 1:length(fruit_names)){
  image_names = list.files(paste0(fruit_names[i],"/"))
  #image_names = list.files(paste0(fruit_names[i],"/"), pattern = "^[0-9]")
  #image_names = list.files(paste0(fruit_names[i],"/"), pattern = "^r")
  list_of_images = lapply(paste0(fruit_names[i],"/",image_names), image_load, target_size = ts[1:2])
  list_of_images = lapply(list_of_images, image_to_array)
  image_matrix = do.call('cbind', lapply(list_of_images, as.numeric))
  image_matrix = image_matrix*(1/255)
  image_matrix[image_matrix > 0.98] = NaN #Remove white!
  for (j in 1:length(list_of_images)){
    counter = counter + 1
    r = image_matrix[1:pixels,j]
    g = image_matrix[pixels+1:pixels*2,j]
    b = image_matrix[(2*pixels+1):(pixels*3),j]
    dim(r) = dim(g) = dim(b) = c(size,size)
    d1[counter,2:5] = c(mean(r[0:(size/2), 0:(size/2)], na.rm = T), mean(r[0:(size/2), ((size/2)+1):size],na.rm = T),
                        mean(r[((size/2)+1):size, 0:(size/2)], na.rm = T), mean(r[((size/2)+1):size, ((size/2)+1):size], na.rm = T))
    d1[counter,6:9] = c(mean(g[0:(size/2), 0:(size/2)], na.rm = T), mean(g[0:(size/2), ((size/2)+1):size],na.rm = T),
                        mean(g[((size/2)+1):size, 0:(size/2)], na.rm = T), mean(g[((size/2)+1):size, ((size/2)+1):size], na.rm = T))
    d1[counter,10:13] = c(mean(b[0:(size/2), 0:(size/2)], na.rm = T), mean(b[0:(size/2), ((size/2)+1):size],na.rm = T),
                        mean(b[((size/2)+1):size, 0:(size/2)], na.rm = T), mean(b[((size/2)+1):size, ((size/2)+1):size], na.rm = T))
    d1[counter,1] = fruit_names[i]
  }
  progress = i/length(fruit_names)
  print(progress)
}
end = Sys.time()
duration = end - start
duration


        
saveRDS(d1,
        file = "/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/Multinomial Logistic Regression/Models/RGB_quadrants/Design Matrices/mmlr_test_all_size50")
