
##CALLING THE DATA

rm(list=ls())
data <- read.csv(file = "D:/RKMVERI MSc BDA/PDS project/Vaccination.csv",header = TRUE)
head(data)


##PREPARING THE DATA TO ANALYZE

colnames(data) <- c("TimeStamp","Age","Gender","Living_area","Vac_safe","Vac_status","Travel","1st_choice",
                    "2nd_choice","3rd_choice","4th_choice","5th_choice","6th_choice","Vac_brand",
                    "Vac_reliable","CoWin","Vac_app_easy","waiting_time","Social_dis","Hygiene",
                    "Vac_symptom","Govt_effort")
library(dplyr)
data$Living_area <- as.numeric(as.factor(data$Living_area))
data$Living_area
hello
