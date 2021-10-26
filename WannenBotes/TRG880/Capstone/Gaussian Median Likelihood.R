library(OpenImageR)
library(matrixStats)
library(mvtnorm)
library(writexl)

linux_fp_train = "/home/rian/Documents/Fruit360/Fruit-Images-Dataset-master/Training"
windows_fp_train = "C:/Users/rianh/OneDrive - Rocketmine Pty Ltd/Documents/Fruit-Images-Dataset-master/Training"
setwd(linux_fp_train)
n = 100
pixels = n*n
r_indx = 1:pixels
g_indx = pixels+1:pixels*2
b_indx = (2*pixels+1):(pixels*3)

fruit_names = list.files()
labels = fruit_names
#Importing all the images

#Model parameters have already been trained. No need to run the below code again,
#simply load the model.
start = Sys.time()
parameters = array(NA, dim = c(pixels, 2, length(labels)))
for (i in 1:length(fruit_names)){
  #image_names = list.files(paste0(fruit_names[i],"/"))
  image_names = list.files(paste0(fruit_names[i],"/"), pattern = "^[0-9]")
  #image_names = list.files(paste0(fruit_names[i],"/"), pattern = "^r")
  list_of_images = lapply(paste0(fruit_names[i],"/",image_names), readImage)
  image_matrix = do.call('cbind', lapply(list_of_images, as.numeric))
  colr = (image_matrix[1:pixels,] + image_matrix[pixels+1:pixels*2,] +
               image_matrix[(2*pixels+1):(pixels*3),])/3
  colr[colr > 0.98] = NaN #Remove white!

  parms = cbind(rowMeans(colr, na.rm = T), rowSds(colr, na.rm = T))
  parameters[,,i] = parms
}
end = Sys.time()
duration = end-start
duration
#saveRDS(parameters,
      #  file = "/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/GMP_upright_50.rds")

#Individual test image upload

linux_fp_parameters = "/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/GML/GMP_upright.rds"
#windows_fp_parameters = "C:/Users/rianh/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/GMP_nw.rds"
parameters = readRDS(linux_fp_parameters)


linux_fp_test = "/home/rian/Documents/Fruit360/Fruit-Images-Dataset-master/Train"
#windows_fp_test = "C:\Users\rianh\OneDrive - Rocketmine Pty Ltd\Documents\Fruit-Images-Dataset-master\Test"
setwd(linux_fp_test)
test = readImage("Avocado/45_100.jpg")
EBImage::display(test, method="browser")
  
IDFruit = function(test_image){
  test_image = EBImage::resize(test_image, w = 100, h = 100)
  test_image = do.call("cbind", list(as.numeric(test_image))) 
  test_image = (test_image[r_indx] + test_image[g_indx] + 
            test_image[b_indx])/3
  test_image[test_image == 1] = 0.99999
  results = matrix(NA, length(labels), 2, dimnames = list(c(labels)))
  for (pset in 1:length(labels)){
    curr_params = parameters[,,pset]
    probs = matrix(NA, pixels)
    for (j in 1:pixels){
      probs[j] = dnorm(test_image[j], mean = curr_params[j,1], 
                         sd = curr_params[j,2])
    }
    results[pset,] = c(mean(probs), median(probs))
  }
  top_5 = tail(results[order(results[,2]),])
  row.names(top_5)[6]
  #return(which(results[,2] == max(results[,2])))
  return(top_5)
}

#Accuracy
accuracy_results = matrix(NA, length(fruit_names), 2)
 for (i in 1:length(fruit_names)){
  image_names = list.files(paste0(fruit_names[i],"/"))
  #image_names = list.files(paste0(fruit_names[i],"/"),pattern = "^r")
  #image_names = list.files(paste0(fruit_names[i],"/"),pattern = "^[0-9]")
  list_of_images = lapply(paste0(fruit_names[i],"/",image_names), readImage)
  
      accuracy_vec = matrix(0, length(image_names), 8, 
                            dimnames = list(c(), c("Indicator", "True", "Predicted", 
                                                   "True_top_5", "Predicted_mean", "Predicted_median", 
                                                   "True_mean", "True_median")))
      
      
      for (j in 1:length(image_names)){
      test_image = list_of_images[[j]]
      test_image = EBImage::resize(test_image, w = 100, h = 100)
      test_image = do.call("cbind", list(as.numeric(test_image))) 
      test_image = (test_image[r_indx] + test_image[g_indx] + 
                      test_image[b_indx])/3
      test_image[test_image > 0.98] = NaN
      
      results = matrix(NA, length(labels), 2, dimnames = list(c(labels)))
      for (pset in 1:length(labels)){
        curr_params = parameters[,,pset]
        probs = matrix(NA, pixels)
        for (jj in 1:pixels){
          probs[jj] = dnorm(test_image[jj], mean = curr_params[jj,1], 
                           sd = curr_params[jj,2])
        }
        results[pset,] = c(mean(probs, na.rm = T), median(probs, na.rm = T))
      }
      
      top_5 = tail(results[order(results[,2]),])
      accuracy_vec[j,2:8] = c(fruit_names[i], row.names(top_5)[6], 
                              length(which(row.names(top_5) == fruit_names[i])) == 1,
                              unname(results[which(row.names(results) == row.names(top_5)[6]),1]),
                              unname(results[which(row.names(results) == row.names(top_5)[6]),2]),
                              unname(results[which(row.names(results) == fruit_names[i]),1]),
                              unname(results[which(row.names(results) == fruit_names[i]),2]))
      if (row.names(top_5)[6] == fruit_names[i]){
        accuracy_vec[j,1] = 1
      }
      }
  accuracy_results[i,] = c(fruit_names[i], mean(as.numeric(accuracy_vec[,1])))
  
  write_xlsx(data.frame(accuracy_vec), 
             paste0("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/GML/GML Accuracy (No white, rotated, median only)/",
                    fruit_names[i],".xlsx"))
  # write_xlsx(data.frame(results), 
  #            paste0("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/GMP Accuracy (No white, median only)/",
  #                   fruit_names[i],"_results",".xlsx"))
  # 
  
  progress = i/length(fruit_names)
  print(progress)
  print(accuracy_results[i,])
}

write_xlsx(data.frame(accuracy_results),
           paste0("/home/rian/Dropbox/2. TRG880/1. Assignments/Capstone/Models/Working models/GML/GML Accuracy (No white, upright, median only)/1_final_results_TRAIN(fit).xlsx"))
mean(as.numeric(accuracy_results[,2]))


