
rm(list = ls())

dir <- dir("coeff/")

names_target <- gsub("coeff_", "", dir)
names_target <- gsub(".RData", "", names_target)
names_target <- c(names_target[5:16], names_target[1:4])







