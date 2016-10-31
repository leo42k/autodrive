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
length(index_test)
for (i in 1:15) {
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

resulttemp <- dlmFilter(record[,1], dlmModPoly())

plot(resulttemp$m[-1,1], type = "l")
points(record[,1])
# saveRDS(set_train, file.path("temp", "set_train.rds"))
# saveRDS(set_test, file.path("temp", "set_test.rds"))

set_train <- readRDS(file.path("temp", "set_train.rds"))
set_test <- readRDS(file.path("temp", "set_test.rds"))
model <- readRDS(file.path("temp", "model.rds"))



# model <- randomForest(set_train[,2:4], set_train[,1])
mean((predict(model, set_train[,2:4]) - set_train[,1])^2)  #validation MSE
mean((predict(model, set_test[,2:4]) - set_test[,1])^2)  #test MSE
sqrt(25.51)
sqrt(4.46)

mean(angle[index_train])
sd(angle[index_train])
mean(angle[index_test])
sd(angle[index_test])



# saveRDS(model, file.path("temp", "model.rds"))

importance(model)

hist(angle)
sd(angle[index_train])
sd(angle[index_test])


predicted.angle <- predict(model, set_test[,2:4])
predicted.angle.train <- predict(model, set_train[,2:4])

sum((predicted.angle > 0) * 1 == (angle[index_test] > 0) *1 ) / length(predicted.angle)
sum((predicted.angle.train > 0) * 1 == (angle[index_train] > 0) *1 ) / length(predicted.angle.train)




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


write.bib(entry = x, file = "test.bib")
x <- citation("randomForest")






i <- 595
k <- index_train[i]
image_use <- (as.cimg(aperm(image[image_names][k,,,], c(4,3,1,2))))
result <- edges( make.image(image_use[,160:1,1,]), type = "Robertcross")
result <- rgb2grey(result)
plot(image_use)
show.image(result)


refren <- rgb2grey(make.image(image_use[,160:1,1,]))
matrixA <- extract.image(refren)
install.packages("spatialfil")
library(spatialfil)

applyFilter(matrixA, C)

Bx <- matrix(c(1,0,0,-1), 2,2)
Bx

C <- matrix(c(1,0,1,2,0,-2,1,0,1), 3, 3)

temp.M <- extract.image(result)
temp.MM <- (temp.M > quantile(temp.M, 0.97))*1
temp.MM[,135:160] <- 0
show.image(make.image(temp.MM))


plot(image_use)
show.image(result)
show.image(make.image(temp.MM))





log_2 <- H5File(file.path("comma-dataset", "log", "2016-02-11--21-32-47.h5"))
image_2 <- H5File(file.path("comma-dataset", "camera", "2016-02-11--21-32-47.h5"))
log_names_2 <- list.datasets(log_2, recursive = TRUE)
image_names_2 <- list.datasets(image_2, recursive = TRUE)




k2 <- 3000
image_use_2 <- (as.cimg(aperm(image_2[image_names_2][k2,,,], c(4,3,1,2))))
result_2 <- edges( make.image(image_use_2[,160:1,1,]), type = "Robertcross")
result_2 <- rgb2grey(result_2)
plot(image_use_2)
show.image(result_2)

temp.M_2 <- extract.image(result_2)
temp.MM_2 <- (temp.M_2 > quantile(temp.M_2, 0.97))*1
temp.MM_2[,135:160] <- 0
show.image(make.image(temp.MM_2))


par(mfrow = c(2,3))

show.image(make.image(rescale(image_use[,160:1,1,], to = c(0, 65535))))
title("daytime image")
show.image(result)
title("Roberts cross filter")
show.image(make.image(temp.MM))
title("detected lane markings")

show.image(make.image(rescale(image_use_2[,160:1,1,], to = c(0, 65535), from = c(0, 255))))
title("nighttime image")
show.image(result_2)
title("Roberts cross filter")
show.image(make.image(temp.MM_2))
title("detected lane markings", sub = "with 97% quantile threshold")



par(mfrow = c(1,1))

P <- matrix(0, 320, 320)
P[temp] <- 1
p <- P
length.row <- nrow(p)
abc=hough(p)
houghParam<-unlist(abc$Header)
image(p,main="Hough lines (blue) and vanishing point (red)", col = gray(c(0, 1)))

P1 <- matrix(0, 320, 320)
P1[temp1] <- 1
getLineHough_plotline_blue(P1)
P2 <- matrix(0, 320, 320)
P2[temp2] <- 1
getLineHough_plotline_blue(P2)

point <- get.vanishing.point(k)

AAA <- get.vanishing.point.plot(k)
AAAA <- cbind(1-AAA[,1], AAA[,2] - 0.5)
points(AAAA[!is.na(AAAA[,1]),1], AAAA[!is.na(AAAA[,1]),2], pch = 20, col = "yellow", cex = 0.2)
points(point[1]+0.5, point[2], pch = 19, col = "red")



