setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# this is for local running; delete it for cluster file
setwd("..")  #always keep thes
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
library(RcppArmadillo)
library(adimpro)
library(randomForest)


source(file.path("code", "draw_path_on.R"))
source(file.path("code", "hough.R"))

log <- H5File(file.path("comma-dataset", "log", "2016-06-08--11-46-01.h5"))
image <- H5File(file.path("comma-dataset", "camera", "2016-06-08--11-46-01.h5"))
log_names <- list.datasets(log, recursive = TRUE)
image_names <- list.datasets(image, recursive = TRUE)

temp <- log[log_names[28]]@dim
speed <- log[log_names[28]][1:temp]
rm(temp)
temp <- log[log_names[35]]@dim
angle <- log[log_names[35]][1:temp]
rm(temp)
angle <- angle/10
temp <- log[log_names[13]]@dim
timeline_image <- log[log_names[13]][1:temp]
rm(temp)
# speed, angle, timeline

index_train <- 11000:13000
index_test <- 13001:14400

length(index_train)
length(index_test)

# angle ~ speed + point[1] + point[2]

set_train <- matrix(0, length(index_train), 4)
for (i in 1:length(index_train)) {
    set_train[i,1] <- angle[min(which(timeline_image == index_train[i]))]
    set_train[i,2] <- speed[min(which(timeline_image == index_train[i]))]
    
}
record <- NULL
for (i in 1:length(index_train)) {
    k <- index_train[i]
    point <- get.vanishing.point(k)
    record <- rbind(record, point)
    ts1 <- ts(record[!is.na(record[,1]), 1])
    ts2 <- ts(record[!is.na(record[,1]), 2])
    ind <- length(ts1)
    if (ind >1) {
        result <- dlmFilter(ts1, dlmModPoly())
        result2 <- dlmFilter(ts2, dlmModPoly())
        new_point <- c(result$m[ind+1,1], result2$m[ind+1,1])
        set_train[i, 3:4] <- new_point
    } else {
        set_train[i, 3:4] <- record
    }
}

set_test <- matrix(0, length(index_test), 4)
for (i in 1:length(index_train)) {
    set_test[i,1] <- angle[min(which(timeline_image == index_test[i]))]
    set_test[i,2] <- speed[min(which(timeline_image == index_test[i]))]
    
}
record <- NULL
for (i in 1:length(index_test)) {
    k <- index_test[i]
    point <- get.vanishing.point(k)
    record <- rbind(record, point)
    ts1 <- ts(record[!is.na(record[,1]), 1])
    ts2 <- ts(record[!is.na(record[,1]), 2])
    ind <- length(ts1)
    if (ind >1) {
        result <- dlmFilter(ts1, dlmModPoly())
        result2 <- dlmFilter(ts2, dlmModPoly())
        new_point <- c(result$m[ind+1,1], result2$m[ind+1,1])
        set_test[i, 3:4] <- new_point
    } else {
        set_test[i, 3:4] <- record
    }
}

# saveRDS(set_train, file.path("temp", "set_train.rds"))
# saveRDS(set_test, file.path("temp", "set_test.rds"))

set_train <- readRDS(file.path("temp", "set_train.rds"))
set_test <- readRDS(file.path("temp", "set_test.rds"))
model <- readRDS(file.path("temp", "model.rds"))

# model <- randomForest(set_train[,2:4], set_train[,1])
mean((predict(model, set_train[,2:4]) - set_train[,1])^2)  #validation MSE
mean((predict(model, set_test[,2:4]) - set_test[,1])^2)  #test MSE

# saveRDS(model, file.path("temp", "model.rds"))


predicted.angle <- predict(model, set_test[,2:4])
predicted.angle.train <- predict(model, set_train[,2:4])


saveVideo(for (i in 1:length(index_test)) {
    k <- index_test[i]
    plot(as.cimg(aperm(image[image_names][k,,,], c(4,3,1,2))))
    draw_path_on(speed_ms = set_test[i, 2], angle_steers = set_test[i,1], color = "blue")
    draw_path_on(speed_ms = set_test[i, 2], angle_steers = predicted.angle[i], color = "green")
    points((set_test[i,3] + 0.5) * 320, 160 - ((set_test[i,4] + 0.5) * 320 - 160), col = "red", pch=19, cex=1.5)
}, "test.mp4", interval = 0.05)

saveVideo(for (i in 1:length(index_train)) {
    k <- index_train[i]
    plot(as.cimg(aperm(image[image_names][k,,,], c(4,3,1,2))))
    draw_path_on(speed_ms = set_train[i, 2], angle_steers = set_train[i,1], color = "blue")
    draw_path_on(speed_ms = set_train[i, 2], angle_steers = predicted.angle.train[i], color = "green")
    points((set_train[i,3] + 0.5) * 320, 160 - ((set_train[i,4] + 0.5) * 320 - 160), col = "red", pch=19, cex=1.5)
}, "train.mp4", interval = 0.05)



i <- 595
k <- index_train[i]
plot(as.cimg(aperm(image[image_names][k,,,], c(4,3,1,2))))
draw_path_on(speed_ms = set_train[i, 2], angle_steers = set_train[i,1], color = "blue")
draw_path_on(speed_ms = set_train[i, 2], angle_steers = predicted.angle.train[i], color = "green")
points((set_train[i,3] + 0.5) * 320, 160 - ((set_train[i,4] + 0.5) * 320 - 160), col = "red", pch=19, cex=1.5)
