library("h5")
library(imager)
library(scales)
library(randomForest)
library(abind)
library(arrayhelpers)
# install.packages("arrayhelpers")
# install.packages("randomForest")
# install.packages("h5", repos = "http://www.omegahat.org/R", type="source")
# install.packages("/Users/leo42k/Downloads/h5_0.9.2.tgz", repos = NULL, type="source")
log <- H5File(file.path("comma-dataset", "log", "2016-06-08--11-46-01.h5"))
image <- H5File(file.path("comma-dataset", "camera", "2016-06-08--11-46-01.h5"))

log_names <- list.datasets(log, recursive = TRUE)
image_names <- list.datasets(image, recursive = TRUE)

image[image_names[1]]

temp <- log[log_names[13]]@dim
timeline_image <- log[log_names[13]][1:temp]
rm(temp)

set.seed(160920)
index_train <- sample(50001:65000, size = 10000)
index_validation <- setdiff(50001:65000, index_train)
length(index_train)
length(index_validation)
index_test <- 70001:75000
# no validatiindex_trainon; risk of over-fitting
# but this type of quesions usually cause under-fitting

temp <- log[log_names[14]]@dim
accel <- log[log_names[14]][1:temp]
rm(temp)

range(accel)
hist(accel, breaks = 100)
sum(accel > 0.8)
sum(accel < -0.8)
length(accel)

accel_tri <- 1 * (accel > 0.8) - 1 * (accel < - 0.8)
# this is the "answer"


temp <- log[log_names[28]]@dim
speed <- log[log_names[28]][1:temp]
rm(temp)

temp <- log[log_names[35]]@dim
angle <- log[log_names[35]][1:temp]
rm(temp)
angle <- scale(angle)


temp <- log[log_names[36]]@dim
angle_accel <- log[log_names[36]][1:temp]
rm(temp)
angle_accel <- rescale(angle_accel, to = c(-1, 1))
# using the deg/s2; is it appropriate? 


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


model_temp <- randomForest(set_train[,2:31], as.factor(set_train[,1]))

sum(predict(model_temp, set_validation[,2:31]) == set_validation[,1])
sum(predict(model_temp, set_test[,2:31]) == set_test[,1])




h5close(log)
h5close(img)





90870/18177
18177/20/60/60

10000-16000
50000-80000



x <- data[names[13]]
x@dim
x[50000]
x[80000]
range(x[1:90870])
x[20000:20080]
x <- data[names[1]]
x@dim
y <- x[2000:2020,,,]

z <- aperm(y, c(4,3,1,2))
dim(z)
as.cimg(z)
plot(as.cimg(z), frame = 5)


plot(image(x[1,1,,]))
range(x[1:90870])
length(x)



plot(boats)

testmap <- array(0, c(160,320,1,3))
for (i in 1:3) {
    testmap[,,1,i] <- x[1,,,][1,i,,]
}
range(testmap)
range(boats)
dim(x[,,,])
pixmapRGB(x[1,,,])
dim(x[1,,,])


plot.cimg(testmap)
class(boats)
class(testmap)
plot(as.cimg(testmap))
?as.cimg


