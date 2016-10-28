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

getLineHough_plot<-function(p){
    length.row <- nrow(p)
    abc=hough(p)
    houghParam<-unlist(abc$Header)
    par(mfrow=c(1,2))
    image(p,main="original")
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
            abline(v=(rho2+length.row/2)/length.row, lwd=2)
            ab_table[i,] <- c((-rho2+length.row/2)/length.row, 1, 0)
        } else{
            abline((b2+length.row/2-a2*length.row/2)/length.row,a2)
            ab_table[i,] <- c((b2+length.row/2-a2*length.row/2)/length.row, a2, 1)
        }
    }
    image(abc$hData, main="Houghmatrix")
    par(mfrow=c(1,1))
}

getLineHough_plotline<-function(p){
    length.row <- nrow(p)
    abc=hough(p)
    houghParam<-unlist(abc$Header)
#    par(mfrow=c(1,2))
#    image(p,main="original")
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
            abline(v=(rho2+length.row/2)/length.row, lwd=2)
#            ab_table[i,] <- c((-rho2+length.row/2)/length.row, 1, 0)
        } else{
            abline((b2+length.row/2-a2*length.row/2)/length.row,a2)
 #           ab_table[i,] <- c((b2+length.row/2-a2*length.row/2)/length.row, a2, 1)
        }
    }
 #   image(abc$hData, main="Houghmatrix")
#    par(mfrow=c(1,1))
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

getLineHough_plotline_blue<-function(p){
    length.row <- nrow(p)
    abc=hough(p)
    houghParam<-unlist(abc$Header)
    #    par(mfrow=c(1,2))
    #    image(p,main="original")
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
            abline(v=(rho2+length.row/2)/length.row, lwd=2, col = "blue")
            #            ab_table[i,] <- c((-rho2+length.row/2)/length.row, 1, 0)
        } else{
            abline((b2+length.row/2-a2*length.row/2)/length.row,a2, col = "blue")
            #           ab_table[i,] <- c((b2+length.row/2-a2*length.row/2)/length.row, a2, 1)
        }
    }
    #   image(abc$hData, main="Houghmatrix")
    #    par(mfrow=c(1,1))
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




get.vanishing.point.plot <- function(k) {
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
    
    temp_I <- nrow(ab_table)
    int_table <- matrix(0, (temp_I*(temp_I-1)/2), 2)
    for (j in 1:(temp_I*(temp_I-1)/2)) {
        i <- combn(1:temp_I,2)[,j]
        if ((det(cbind(-ab_table[i,2], ab_table[i,3])) != 0 )& (abs(diff(ab_table[i,2])) > 0.2 ))
            int_table[j,] <- solve(cbind(-ab_table[i,2], ab_table[i,3])) %*% ab_table[i,1] else int_table[j,] <- c(NA, NA)
    }
    return(int_table)
}    
    


