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
library(dlm)

log <- h5file(file.path("comma-dataset", "log", "2016-05-12--22-20-00.h5"))
image <- h5file(file.path("comma-dataset", "camera", "2016-06-08--11-46-01.h5"))
log_names <- list.datasets(log, recursive = TRUE)
image_names <- list.datasets(image, recursive = TRUE)
image[image_names]  #check the image we loaded

log_names

video <-  aperm(image[image_names][1:200,,,], c(4,3,1,2))
dim(video)

target <- video[,,1,]
dim(target)
plot(as.cimg(target))


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



dim(image[image_names])
hist(accel_tri)



p <- P

getLineHough<-function(p){
    length.row <- nrow(p)
    abc=hough(p)
    houghParam<-unlist(abc$Header)
    # par(mfrow=c(1,2))
    # image(p,main="original")
    I <- unique(as.vector(abc$hData)) %>% length
    temp_I <- floor(I*0.2) + 1
    ab_table <- matrix(0, temp_I, 3)
    for (i in 1:temp_I) {
        secondPoint_vec<-which(abc$hData==sort(unique(as.vector(abc$hData)), decreasing = T)[i],arr.ind=T)
        secondPoint <- apply(secondPoint_vec, 2, mean)
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
    return(ab_table)
}


getCenter <- function(table) {
    ab_table <- table
    temp_I <- nrow(table)
    int_table <- matrix(0, (temp_I*(temp_I-1)/2), 2)
    for (j in 1:(temp_I*(temp_I-1)/2)) {
        i <- combn(1:temp_I,2)[,j]
        if ((det(cbind(-ab_table[i,2], ab_table[i,3])) != 0 )& (abs(diff(ab_table[i,2])) > 0.2 ))
            int_table[j,] <- solve(cbind(-ab_table[i,2], ab_table[i,3])) %*% ab_table[i,1] else int_table[j,] <- c(NA, NA)
    }
    cord <- colMeans(int_table, na.rm=TRUE)
    return(cord)
}









get.vanishing.point <- function(k) {
    # temp.M <- aperm(image[image_names][k,,,], c(4,3,1,2))  %>% grayscale
    # temp.MM <- (temp.M[,,1,1]>150/256)
    image_use <- (as.cimg(aperm(image[image_names][k,,,], c(4,3,1,2))))
    result <- edges( make.image(image_use[,,1,]), type = "Sobel")
    temp.M <- extract.image(rgb2grey(result))
    temp.MM <- (temp.M > quantile(temp.M, 0.97))*1
    temp <- which(temp.MM == 1, arr.ind = T)
    rm(temp.MM, temp.M)
    
    temp1 <- temp[(temp[,2] > 25)&(temp[,1] <=160), ]
    temp2 <- temp[(temp[,2] > 25)&(temp[,1] >160), ]
#    temp <- temp[(temp[,2] > 25), ]
#    P <- matrix(0, 320, 320)
#    P[320 - temp] <- 1
#    ab_table <- getLineHough(P)    
    
    P <- matrix(0, 320, 320)
    P[320 - temp1] <- 1
    ab_table1 <- getLineHough(P)
    P <- matrix(0, 320, 320)
    P[320 - temp2] <- 1
    ab_table2 <- getLineHough(P)
    ab_table <- rbind(ab_table1, ab_table2)
    point <- getCenter(ab_table) - 0.5
    point[1] <- -point[1]
    if (is.na(point[1])) point <- c(0, -3/8)
    return(point)
}


(13600-10000)/20+1
points <- matrix(0, 181, 2)
ind <- 0
for (k in seq(10000, 13600, by = 20)) {
    ind <- ind + 1
    points[ind, ] <- get.vanishing.point(k)
}




library(adimpro)
plot(result)
result$img [[1]]
result$type
?adimpro


image_use <- grayscale(as.cimg(aperm(image[image_names][k,,,], c(4,3,1,2))))
result <- edges( make.image(image_use[,,1,]), type = "Sobel")
a <- extract.image(result)









record <- NULL


saveVideo(for (k in seq(11000, 14400, 5)) {
    plot(as.cimg(aperm(image[image_names][k,,,], c(4,3,1,2))))
    point <- get.vanishing.point(k)
    record <- rbind(record, point)
    ts1 <- ts(record[!is.na(record[,1]), 1])
    ts2 <- ts(record[!is.na(record[,1]), 2])
    ind <- length(ts1)
    if (ind >1) {
        result <- dlmFilter(ts1, dlmModPoly())
        result2 <- dlmFilter(ts2, dlmModPoly())
        new_point <- c(result$m[ind+1,1], result2$m[ind+1,1])
        points((new_point[1] + 0.5) * 320, 160 - ((new_point[ 2] + 0.5) * 320 - 160), col = "blue", pch=19, cex=1.5)
    } else {
        points((record[1,1] + 0.5) * 320, 160 - ((record[1, 2] + 0.5) * 320 - 160), col = "blue", pch=19, cex=1.5)
    }
    }, "test2.mp4", interval = 0.25)









myFilt <- dlmFilter(ts(points[,1]), dlmModPoly())
new_points <- cbind(as.vector(myFilt$m[-1,1]), points[,2])

new_points


plot(myFilt$m[-1], type = "l", col = "red")
lines(myts)






















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


