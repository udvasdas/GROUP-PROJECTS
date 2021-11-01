
## CALLING THE DATA

rm(list=ls())
data <- read.csv(file = "D:/RKMVERI MSc BDA/PDS project/Vaccination.csv",header = TRUE)
head(data)

## CALLING THE LIBRARIES
library(dplyr)
library(ggplot2)

##PREPARING THE DATA TO ANALYZE
## Columns to be modified--- Living area, Vac_safe, Vac_status, Vac_reliable, CoWin, Vac_symptom   
## Changing the column names to something short for future use.
colnames(data) <- c("TimeStamp","Age","Gender","Living_area","Vac_safe","Vac_status","Travel","1st_choice",
                    "2nd_choice","3rd_choice","4th_choice","5th_choice","6th_choice","Vac_brand",
                    "Vac_reliable","CoWin","Vac_app_easy","waiting_time","Social_dis","Hygiene",
                    "Vac_symptom","Govt_effort")

data$Living_area <- as.numeric(as.factor(data$Living_area))
data$Living_area
##here we are using to represent the entire column "Living_area" as a factor of (1,2,3)
## 1 represents Metropolitan city
## 2 represents Rural area 
##3 is small town.

data$Vac_safe <- as.numeric (as.factor(data$Vac_safe))
##here we are using to represent the entire column "Vaccination_safe" as a factor of (1,2,3)
## 1 represents "No, it can affect my health", 
## 2 represents "Not Sure" 
## 3 is "Yes"

data$Vac_status <- as.numeric (as.factor(data$Vac_status))
table (data$Vac_status)
##here we are using to represent the entire column "Vaccination_status" as a factor of (1,2,3)
## 1 represents "1st dose is taken", 
## 2 represents "Both doses taken" 
## 3 is "Not Vaccinated".

data$Vac_reliable <- as.numeric (as.factor(data$Vac_reliable))
##here we are using to represent the entire column "Vaccination_reliability" as a factor of (1,2,3)
## 1 represents "Free vaccines offered by the Government", 
## 2 represents "Paid vaccines offered by private organizations".

data$CoWin <- as.numeric (as.factor(data$CoWin))
## 1 represents "No, I have not used the portal", 
## 2 represents "Yes, but I did not face any problem", and
## 3 represents "Yes, I faced a lot of issues while booking".

data$Vac_symptom <- as.numeric (as.factor(data$Vac_symptom))
## 1 represents "After 1st dose", 
## 2 represents "After 2nd dose", and
## 3 represents "After both doses"
## 4 represents "No, I did not face any".

## Looking at the data Demographically ____ Plots we need to do initially___
## 1. Age, 2. Gender, 3. Living_area, 4. Living_area wrt Gender and Age using multiple bar chart , 5. Gender based on different age interval using multiple bar chart

## Bar and pie diagram of age composition of the data
tab <- table(data$Age)
df <- data.frame(Age=unique(data$Age),count=c(96,4,1,7))
bar <- ggplot(data=df,aes(y=count,x=Age),beside=T)+geom_bar(stat="identity",width = 0.5)+
  labs(title="Age composition",x="Age Intervals",y="Frequency")
bar

bar1 <- ggplot(data=df,aes(y=count,x="",fill=Age))+geom_bar(stat="identity",width = 0.5)+
  labs(title="Age composition",x="Age Intervals",y="Frequency")
pie <- bar1 + coord_polar("y",start=0)
pie + geom_text(label=paste(df$count),position=position_stack(vjust = 0.5))


##pie diagram for gender composition of the data
table (data$Gender)
df1 <- data.frame(Gender=unique(data$Gender),count=c(37,71))
bar1 <- ggplot (df1, aes(y= count,x= "", fill=Gender))+geom_bar(stat = "identity", width = 0.5)+
  labs(title = "Gender Composition", x= "Gender", y= "frequency")
pie<- bar1+ coord_polar("y", start = 0)
pie + geom_text(label=paste(df1$count),position=position_stack(vjust = 0.5))


##Bar and pie diagram of to categorise the living area of the data
table(data$Living_area)
df <- data.frame(Living=unique(data$Living_area),count=c(63,31,14))
bar <- ggplot(data=df,aes(y=count,x=Living),beside=T)+geom_bar(stat="identity",width = .75)+
  labs(title="Living area",x="Factor",y="Count")+
  annotate("text", x=1.25, y=60, label= "'1' Represent Metroplitan area.",size=3.5)+
  annotate("text", x=1.13, y=57, label= "'2' Represent Rural area.",size=3.5)+
  annotate("text", x=1.15, y=54, label= "'3' Represent Small Town.",size=3.5)
bar

bar1 <- ggplot(data=df,aes(y=count,x="",fill= Living))+geom_bar(stat="identity",width = 0.5)+
  labs(title="Pie diagram to show Living area in the data
  '1' Represent Metroplitan area.
  '2' Represent Rural area.
  '3' Represent Small Town.",y="Frequency")
pie <- bar1 + coord_polar("y",start=0)
pie + geom_text(label=paste(df$count),position=position_stack(vjust = 0.5))
