library("h5")
# install.packages("h5", repos = "http://www.omegahat.org/R", type="source")
install.packages("/Users/leo42k/Downloads/h5_0.9.2.tgz", repos = NULL, type="source")

library(Rcpp)
data <- H5File("/Users/leo42k/Desktop/autodrive/comma-dataset/log/2016-06-08--11-46-01.h5")
data


list.datasets(data, recursive = TRUE)


x <- data["/times"]
x@dim
x[263583]
length(x)




h5close(data)
