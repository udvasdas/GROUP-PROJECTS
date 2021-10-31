
## CALLING THE DATA

rm(list=ls())
data <- read.csv(file = "D:/RKMVERI MSc BDA/PDS project/Vaccination.csv",header = TRUE)
head(data)

## CALLING THE LIBRARIES
library(dplyr)
library(ggplot2)

##PREPARING THE DATA TO ANALYZE
## Columns to be modified--- Living area, Vac_safe, Vac_status, Vac_reliable, CoWin, Vac_symptom   
##changing the column names to something short for future use.
colnames(data) <- c("TimeStamp","Age","Gender","Living_area","Vac_safe","Vac_status","Travel","1st_choice",
                    "2nd_choice","3rd_choice","4th_choice","5th_choice","6th_choice","Vac_brand",
                    "Vac_reliable","CoWin","Vac_app_easy","waiting_time","Social_dis","Hygiene",
                    "Vac_symptom","Govt_effort")

data$Living_area <- as.numeric(as.factor(data$Living_area))
data$Living_area
##here we are using to represent the entire column "living area" as a factor of (1,2,3)
## here 1 represents Metropolitan city, 2 represents Rural area and 3 is small town.

data$Vac_safe <- as.numeric (as.factor(data$Vac_safe))
##here we are using to represent the entire column "Vaccination safe" as a factor of (1,2,3)
## here 1 represents "No, it can affect my health", 
#2 represents "Not Sure" and 3 is "Yes"

data$Vac_status <- as.numeric (as.factor(data$Vac_status))
table (data$Vac_status)
##here we are using to represent the entire column "Vaccination status" as a factor of (1,2,3)
## here 1 represents "1st dose is taken", 
#2 represents "Both doses taken" and 3 is "Not Vaccinated".

data$Vac_reliable <- as.numeric (as.factor(data$Vac_reliable))
##here we are using to represent the entire column "Vaccination reliability" as a factor of (1,2,3)
## here 1 represents "Free vaccines offered by the Government", 
#2 represents "Paid vaccines offered by private organizations".

data$CoWin <- as.numeric (as.factor(data$CoWin))
## here 1 represents "No, I have not used the portal", 
#2 represents "Yes, but I did not face any problem", and
##3 represents "Yes, I faced a lot of issues while booking".

data$Vac_symptom <- as.numeric (as.factor(data$Vac_symptom))
## here 1 represents "After 1st dose", 
#2 represents "After 2nd dose", and
##3 represents "After both doses"
##4 represents "No, I did not face any".

