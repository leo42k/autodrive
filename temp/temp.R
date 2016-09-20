library("h5")
library(imager)
# install.packages("h5", repos = "http://www.omegahat.org/R", type="source")
# install.packages("/Users/leo42k/Downloads/h5_0.9.2.tgz", repos = NULL, type="source")






90870/18177
18177/20/60/60

data <- H5File("/Users/leo42k/Desktop/autodrive/comma-dataset/camera/2016-06-08--11-46-01.h5")
names <- list.datasets(data, recursive = TRUE)
names

x <- data[names[1]]
x@dim
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


h5close(data)
