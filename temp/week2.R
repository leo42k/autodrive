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
plot(as.cimg(aperm(image[image_names][7002,,,], c(4,3,1,2))))  #the end of test set
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

library(MASS)


get.angle(12000)


get.angle <- function(k) {
temp.M <- image[image_names][k,,,]
# temp.MM <- (0.212671*temp.M[1,1,,]+0.715160*temp.M[1,2,,]+0.072169*temp.M[1,3,,]>240)
temp.MM <- t((temp.M[1,1,,]>180)&(temp.M[1,2,,]>180)&(temp.M[1,3,,]>180))
# temp.MM <- t(as.matrix(temp.MM))
temp1 <- which(temp.MM[1:160,] == 1, arr.ind = T)
temp2 <- which(temp.MM[161:320,] == 1, arr.ind = T)
rm(temp.MM, temp.M)
if (length(unique(temp1[,1])) > 1 ) {
    c1 <- coef(fastLmPure(cbind(1,temp1[,1]),temp1[,2]))[2]
} else {
    c1 <- 1
}
if (length(unique(temp2[,1])) > 1 ) {
    c2 <- coef(fastLmPure(cbind(1,temp2[,1]),temp2[,2]))[2]
} else {
    c2 <- 1
}
rm(temp1, temp2)
return(c(c1, c2))
}


**Acceleration status** ~ 10sec **speed** + 10sec **steering angle** + 10sec **angle acceleration**
**angle** ~ 10 sec c1 + 10 sec c2
    
        
set.seed(160926)
index_train <- sample(50001:65000, size = 10000)
index_validation <- setdiff(50001:65000, index_train)
index_test <- 70001:75000

length(index_train)
length(index_validation)
length(index_test)


set_train <- matrix(0, length(index_train), 11)
set_train[,1] <- as.matrix(angle_accel[index_train])
for (j in 1:length(index_train)){
    for (i in 1:5) {
        here <- get.angle(timeline_image[index_train[j]-i*100])
        set_train[j,i + 1] <- here[1]
        set_train[j,i + 6] <- here[2]
    }
}

library(RcppArmadillo)

set_validation <- matrix(0, length(index_validation), 11)
set_validation[,1] <- as.matrix(angle_accel[index_validation])

for (j in 1:length(index_validation)){
    for (i in 1:5) {
        here <- get.angle(timeline_image[index_validation[j]-i*100])
        set_validation[j,i + 1] <- here[1]
        set_validation[j,i + 6] <- here[2]
    }
}

set_test <- matrix(0, length(index_test), 11)
set_test[,1] <- as.matrix(angle_accel[index_test])
for (j in 1:length(index_test)){
    for (i in 1:5) {
        here <- get.angle(timeline_image[index_test[j]-i*100])
        set_test[j,i + 1] <- here[1]
        set_test[j,i + 6] <- here[2]
    }
}


saveRDS(set_train, file.path("temp", "set_train_angle.rds"))
saveRDS(set_validation, file.path("temp", "set_validation_angle.rds"))
saveRDS(set_test, file.path("temp", "set_test_angle.rds"))


library(randomForest)
# train <- data.frame(y = set_train[,1], x = set_train[,2:11])
# validation <- data.frame(y = set_validation[,1], x = set_validation[,2:11])
# test <- data.frame(y = set_test[,1], x = set_test[,2:11])
model <- randomForest(set_train[,2:11], set_train[,1])

# model <- lm(y~., train)
# mean((predict(model, validation) - validation[,1])^2)
# mean((predict(model, test) - test[,1])^2)
# mean((predict(model, train) - train[,1])^2)

sqrt(mean((predict(model, set_validation[,2:11]) - set_validation[,1])^2) )  #validation MSE
sqrt(mean((set_validation[,1])^2))

hist(predict(model, set_validation[,2:11]) )
hist(predict(model, set_test[,2:11]) )

sqrt(mean((predict(model, set_test[,2:11]) - set_test[,1])^2) )  #test MSE
mean(set_test[,1])

hist(set_validation[,1])
hist(set_test[,1])





set_train <- readRDS(file.path("temp", "set_train_angle.rds"))
set_validation <- readRDS(file.path("temp", "set_validation_angle.rds"))
set_test <- readRDS(file.path("temp", "set_test_angle.rds"))

















set.seed(160926)
index_train <- sample(50001:65000, size = 10000)
index_validation <- setdiff(50001:65000, index_train)
index_test <- 70001:75000

length(index_train)
length(index_validation)
length(index_test)


set_train <- matrix(0, length(index_train), 11)
set_train[,1] <- as.matrix(angle_accel[index_train])
for (j in 1:length(index_train)){
    for (i in 1:5) {
        here <- get.angle(timeline_image[index_train[j]-i*100])
        set_train[j,i + 1] <- here[1]
        set_train[j,i + 6] <- here[2]
    }
}

library(RcppArmadillo)

set_validation <- matrix(0, length(index_validation), 11)
set_validation[,1] <- as.matrix(angle_accel[index_validation])
for (j in 1:length(index_validation)){
    for (i in 1:5) {
        here <- get.angle(timeline_image[index_validation[j]-i*100])
        set_validation[,i + 1] <- here[1]
        set_validation[,i + 6] <- here[2]
    }
}

set_test <- matrix(0, length(index_test), 11)
set_test[,1] <- as.matrix(angle_accel[index_test])
for (j in 1:length(index_test)){
    for (i in 1:5) {
        here <- get.angle(timeline_image[index_test[j]-i*100])
        set_test[,i + 1] <- here[1]
        set_test[,i + 6] <- here[2]
    }
}

model <- randomForest(set_train[,2:11], set_train[,1])
mean((predict(model, set_validation[,2:11]) - set_validation[,1])^2)  #validation MSE
mean((predict(model, set_test[,2:11]) - set_test[,1])^2)  #test MSE

hist(set_validation[,1])
hist(set_test[,1])


