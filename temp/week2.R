setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# this is for local running; delete it for cluster file
setwd("..")  #always keep thes
    
list.files(file.path("comma-dataset"), recursive = T)

library("h5")
log <- H5File(file.path("comma-dataset", "log", "2016-06-08--11-46-01.h5"))
image <- H5File(file.path("comma-dataset", "camera", "2016-06-08--11-46-01.h5"))
log_names <- list.datasets(log, recursive = TRUE)
image_names <- list.datasets(image, recursive = TRUE)
image[image_names]  #check the image we loaded

range(image[image_names][1,,,])

library(imager)
par(mfrow = c(2, 2))  
plot(as.cimg(aperm(image[image_names][10001,,,], c(4,3,1,2))))  #the start of training set
plot(as.cimg(aperm(image[image_names][13001,,,], c(4,3,1,2))))  #the end of training set
plot(as.cimg(aperm(image[image_names][14002,,,], c(4,3,1,2))))  #the start of test set
plot(as.cimg(aperm(image[image_names][15002,,,], c(4,3,1,2))))  #the end of test set
par(mfrow = c(1, 1))  

log_names


# ```{r speed}
temp <- log[log_names[28]]@dim
speed <- log[log_names[28]][1:temp]
rm(temp)
hist(speed)


# ```{r accel}
temp <- log[log_names[14]]@dim
accel <- log[log_names[14]][1:temp]
rm(temp)
range(accel)
hist(accel, breaks = 100)
# ```{r  accel cont}
sum(accel > 0.8)
sum(accel < -0.8)
length(accel)
accel_tri <- 1 * (accel > 0.8) - 1 * (accel < - 0.8)  #this is the "answer" for supervised learning

library(scales)

temp <- log[log_names[35]]@dim
angle <- log[log_names[35]][1:temp]
rm(temp)
angle <- scale(angle)
temp <- log[log_names[36]]@dim
angle_accel <- log[log_names[36]][1:temp]
rm(temp)
angle_accel <- rescale(angle_accel, to = c(-1, 1))

hist(angle)
hist(angle_accel)

temp <- log[log_names[13]]@dim
timeline_image <- log[log_names[13]][1:temp]
rm(temp)
length(timeline_image)
range(timeline_image)



hist(accel_tri)









**Acceleration status** ~ 10sec **speed** + 10sec **steering angle** + 10sec **angle acceleration**
    
    ### 2.1 Training, Validation and Testing set
    
    ```{r}
set.seed(160920)
index_train <- sample(50001:65000, size = 10000)
index_validation <- setdiff(50001:65000, index_train)
index_test <- 70001:75000

set_train <- matrix(0, 10000, 31)
set_train[,1] <- as.matrix(accel_tri[index_train])
for (i in 1:10) {
    set_train[,i + 1] <- speed[index_train-i*100]
    set_train[,i + 11] <- angle[index_train-i*100]
    set_train[,i + 21] <- angle_accel[index_train-i*100]
}


set_validation <- matrix(0, 5000, 31)
set_validation[,1] <- as.matrix(accel_tri[index_validation])
for (i in 1:10) {
    set_validation[,i + 1] <- speed[index_validation-i*100]
    set_validation[,i + 11] <- angle[index_validation-i*100]
    set_validation[,i + 21] <- angle_accel[index_validation-i*100]
}

set_test <- matrix(0, 5000, 31)
set_test[,1] <- as.matrix(accel_tri[index_test])
for (i in 1:10) {
    set_test[,i + 1] <- speed[index_test-i*100]
    set_test[,i + 11] <- angle[index_test-i*100]
    set_test[,i + 21] <- angle_accel[index_test-i*100]
}
```

### 2.2 Fitting with random forests

```{r, message=FALSE}
library(randomForest)
model_naive <- randomForest(set_train[,2:31], as.factor(set_train[,1]))

sum(predict(model_naive, set_validation[,2:31]) == set_validation[,1])/5000  #validation
sum(predict(model_naive, set_test[,2:31]) == set_test[,1])/5000  #test
```

The validation set is contaminated. 

```{r}
sum(predict(model_naive, set_test[set_test[,1] == 0 ,2:31]) == set_test[set_test[,1] == 0 ,1])/sum(set_test[,1] == 0)
sum(predict(model_naive, set_test[set_test[,1] == 1 ,2:31]) == set_test[set_test[,1] == 1 ,1])/sum(set_test[,1] == 1)
sum(predict(model_naive, set_test[set_test[,1] == -1 ,2:31]) == set_test[set_test[,1] == -1 ,1])/sum(set_test[,1] == -1)
```

So we can see that the image data is needed. 

## 3. Plan this week

The project is difficult in two meanings: 
    
    - We need to deal with image data;
- Convolutional learning is always difficult.

In addition, we will need to predict acceleration and steering angle in the end. 

#### Tasks:

- Example codes for angle prediction; visualizing the results

- Simple ideas for image data: stamping out by grid regions; outlier detection

- Papers regarding convolutional learning

- A slightly improved model with images involved

```{r}
M <- aperm(image[image_names][13001,,,], c(4,3,1,2))
MM <- M[,,1,1]>=235
MMM <- M
for (i in 1:3) {
    MMM[,,1,i] <- MM*M[,,1,i]
}
plot(as.cimg(MMM))
plot(as.cimg(M))
```

- Angle prediction could be easier
- Any data on distance to the car in front?