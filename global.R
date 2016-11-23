
rm(list = ls())

dir <- dir("coeff/")

names_target <- gsub("coeff_", "", dir)
names_target <- gsub(".RData", "", names_target)
names_target <- c(names_target[5:16], names_target[1:4])

load("data_sd_residials.RData")

data_sd_residials$lm_name <- as.character(data_sd_residials$lm_name)

data_sd_residials$lm_name <- gsub("fit_", "", data_sd_residials$lm_name)
data_sd_residials$lm_name <- gsub("_step.RData", "", data_sd_residials$lm_name)


#coeff_list <- list()
for(i in 1:length(dir)){
  #i = 1
  filename_ <- paste0("coeff/", dir[i])
  load(filename_)
  coefficients_list_ <- list(coefficients)
  if(i == 1) coefficients_list <- coefficients_list_
  else coefficients_list <- c(coefficients_list, coefficients_list_)
}

str(coefficients_list)

