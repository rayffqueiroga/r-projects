rm(list=ls())
require(EBImage)


setwd("C://vc//data_set_moedas//moedas_resized//moedas_50cent_resized")


out_file <- "C://vc//data_set_moedas//moedas_resized//moedas_50cent_resized//moedas_50cent.csv"

images <- list.files()


df <- data.frame()


img_size <- 28*28

# label
label <- 50


for(i in 1:length(images))
{
    img <- readImage(images[i])
    img_matrix <- img@.Data
    img_vector <- as.vector(t(img_matrix))
    vec <- c(label, img_vector)
    df <- rbind(df,vec)
    print(paste("Done ", i, sep = ""))
}


names(df) <- c("label", paste("pixel", c(1:img_size)))


write.csv(df, out_file, row.names = FALSE)

#-------------------------------------------------------------------------------
# Test and train split and shuffle



setwd("C://vc//data_set_moedas//data_process//")

moeda_1real <- read.csv("C://vc//data_set_moedas//moedas_resized//moedas_1real_resized//moedas_1real.csv")
moeda_50cent <- read.csv("C://vc//data_set_moedas//moedas_resized//moedas_50cent_resized//moedas_50cent.csv")
moeda_25cent <- read.csv("C://vc//data_set_moedas//moedas_resized//moedas_25cent_resized//moedas_25cent.csv")
moeda_10cent <- read.csv("C://vc//data_set_moedas//moedas_resized//moedas_10cent_resized//moedas_10cent.csv")
moeda_5cent <- read.csv("C://vc//data_set_moedas//moedas_resized//moedas_5cent_resized//moedas_5cent.csv")


new <- rbind(moeda_1real, moeda_50cent, moeda_25cent, moeda_10cent, moeda_5cent)


shuffled <- new[sample(1:1597),]

# Train-test split
train <- shuffled[1:1200,]
test <- shuffled[1201:1597,]


write.csv(train, "train.csv",row.names = FALSE)
write.csv(test, "test.csv",row.names = FALSE)