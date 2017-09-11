rm(list=ls())
require(EBImage)


setwd("C://vc//data_set_moedas//moedas//moedas_50cent")

save_in <- "C://vc//data_set_moedas//moedas_resized//moedas_50cent_resized//"


images <- list.files()

w <- 28

h <- 28


for(i in 1:length(images))
{

    result <- tryCatch({

    imgname <- images[i]

    img <- readImage(imgname)

    img_resized <- resize(img, w = w, h = h)

    grayimg <- channel(img_resized,"gray")
	
	highimg <- matrix(1, nc = 3, nr = 3)
	
	highimg[2, 2] <- -8
	
	imghigh <- filter2(grayimg, highimg)

    path <- paste(save_in, imgname, sep = "")

    writeImage(imghigh, path, quality = 70)

    print(paste("Done",i,sep = " "))},

    error = function(e){print(e)})
}

