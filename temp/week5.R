setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# this is for local running; delete it for cluster file
setwd("..")  #always keep thes
getwd()

library(imager)
library(dplyr)
library(animation)
library(PET)
library(raster)
library(scales)
library(MASS)
library(pracma)  # cot


library("h5")
log <- h5file(file.path("comma-dataset", "log", "2016-05-12--22-20-00.h5"))
image <- h5file(file.path("comma-dataset", "camera", "2016-06-08--11-46-01.h5"))
log_names <- list.datasets(log, recursive = TRUE)
image_names <- list.datasets(image, recursive = TRUE)
image[image_names]  #check the image we loaded

log_names
# temp <- log[log_names[39]]
# temp[15000:16000,]

# range(log[log_names[40]][1:20000,])
# 4095*2

# range(image[image_names][1,,,])


video <-  aperm(image[image_names][1:200,,,], c(4,3,1,2))

# install.packages("animation")


dim(video)

target <- video[,,1,]
dim(target)
plot(as.cimg(target))



p=matrix(diag(100), 100)

r <- raster(p)
plot(r)
dim(r)
abc=hough(p)


p.g <- grayscale( aperm(image[image_names][200,,,], c(4,3,1,2)))
plot(p.g)
p.g <- p.g[,,1,1]
dim(p.g)
range(p.g)

hough(p.g)

P <- p

hP <- hough(P)
viewData(list(P, hP$hData), list("Phantom", "Hough transformed phantom"))
rm(P,hP)

saveVideo(plot(video[,,,1]), "test.mp4")
plot(video[1,,,])
plot(as.cimg(aperm(image[image_names][10001,,,], c(4,3,1,2))))  #the start of training set
plot(as.cimg(aperm(image[image_names][13001,,,], c(4,3,1,2))))  #the end of training set
plot(as.cimg(aperm(image[image_names][14002,,,], c(4,3,1,2))))  #the start of test set
plot(as.cimg(aperm(image[image_names][6000,,,], c(4,3,1,2))))  #the end of test set
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





getLineHough<-function(p){
    length.row <- nrow(p)
    abc=hough(p)
    houghParam<-unlist(abc$Header)
    # par(mfrow=c(1,2))
    # image(p,main="original")
    I <- unique(as.vector(abc$hData)) %>% length
    ab_table <- matrix(0, floor(I*0.2), 3)
    for (i in 1:floor(I*0.2)) {
        secondPoint<-which(abc$hData==sort(unique(as.vector(abc$hData)), decreasing = T)[i],arr.ind=T)
        secondPoint<-apply(secondPoint,2,mean)
        theta2=(secondPoint[1]-1)*houghParam["DeltaXY1"]+houghParam["XYmin1"]
        rho2=(secondPoint[2]-1)*houghParam["DeltaXY2"]+houghParam["XYmin2"]
        a2<--cot(theta2)
        b2<-rho2/sin(theta2)
        if(theta2==0){
            #            abline(v=(rho2+length.row/2)/length.row, lwd=2)
            ab_table[i,] <- c((-rho2+length.row/2)/length.row, 1, 0)
        } else{
            #            abline((b2+length.row/2-a2*length.row/2)/length.row,a2)
            ab_table[i,] <- c((b2+length.row/2-a2*length.row/2)/length.row, a2, 1)
        }
    }
    #    image(abc$hData, main="Houghmatrix")
    temp_I <- floor(I*0.2)
    int_table <- matrix(0, (temp_I*(temp_I-1)/2), 2)
    for (j in 1:(temp_I*(temp_I-1)/2)) {
        i <- combn(1:temp_I,2)[,j]
        if ((det(cbind(-ab_table[i,2], ab_table[i,3])) != 0 )& (abs(diff(ab_table[i,2])) > 0.1 ))
            int_table[j,] <- solve(cbind(-ab_table[i,2], ab_table[i,3])) %*% ab_table[i,1] else int_table[j,] <- c(NA, NA)
    }
    cord <- colMeans(int_table, na.rm=TRUE)
    return(cord)
}



get.vanishing.point <- function(k) {
    k <- 7500
    temp.M <- aperm(image[image_names][k,,,], c(4,3,1,2))  %>% grayscale
    temp.MM <- (temp.M[,,1,1]>128/256)
    temp <- which(temp.MM == 1, arr.ind = T)
    rm(temp.MM, temp.M)
    temp <- temp[temp[,2] > 40,]
    P <- matrix(0, 320, 320)
    P[320 - temp] <- 1
    point <- getLineHough(P) - 0.5
    return(point)
}


hP <- hough(P)
viewData(list(P, hP$hData), list("Phantom", "Hough transformed phantom"))
range(hP$hData)
rm(P,hP)

install.packages("pracma")
library(pracma)

a=matrix(rep(0,10000), 100, 100)
for (i in 1:100)
{a[i,60]=1
}

d=matrix(rep(0,10000), 100, 100)
for (i in 1:100)
{d[60,i]=1
}

e=matrix(diag(100), 100)

ab_table <- NULL



ab_table[1,1:2]







int_table

cord
points(cord[1], cord[2], col = "blue")

points(0.5, 0.5, col="blue")

getLineHough(a)
getLineHough(d)
getLineHough(e)
getLineHough(P, NULL)
ab_table


ab_table








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


