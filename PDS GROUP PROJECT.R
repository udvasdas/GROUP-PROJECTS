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
## 1. Age, 2. Gender, 3. Living_area, 4. Gender based on different age interval using multiple bar chart, 5. Living_area wrt Gender and Age using multiple bar chart 

## Bar and pie diagram of age composition of the data
df <- data.frame(table(data$Age))
bar <- ggplot(data=df,aes(y=Freq,x=Var1),beside=T)+geom_bar(stat="identity",width = 0.5)+
  labs(title="Age composition",x="Age Intervals",y="Frequency")
bar

bar1 <- ggplot(data=df,aes(y=Freq,x="",fill=Var1))+geom_bar(stat="identity",width = 0.5)+
  labs(title="Age composition",x="Age Intervals",y="Frequency")
pie <- bar1 + coord_polar("y",start=0)
pie + geom_text(label=paste(df$Freq),position=position_stack(vjust = 0.5))


##pie diagram for gender composition of the data
df1 <- data.frame(table(data$Gender))
bar1 <- ggplot (df1, aes(y = Freq,x = "", fill= Var1))+geom_bar(stat = "identity", width = 0.5)+
  labs(title = "Gender Composition", x= "Gender", y= "Frequency")
pie<- bar1+ coord_polar("y", start = 0)
pie + geom_text(label=paste(df1$Freq),position=position_stack(vjust = 0.5))




##Bar and pie diagram of to categorize the living area of the data
table(data$Living_area)
df <- data.frame(Living=unique(data$Living_area),count=c(63,31,14))
bar <- ggplot(data=df,aes(y=count,x=Living),beside=T)+geom_bar(stat="identity",width = .75)+
  labs(title="Living area",x="Factor",y="Count")+
  annotate("text", x=1.19, y=60, label= "'1' Represents Metroplitan area.",size=3.5)+
  annotate("text", x=1.11, y=57, label= "'2' Represents Rural area.",size=3.5)+
  annotate("text", x=1.13, y=54, label= "'3' Represents Small Town.",size=3.5)
bar

bar1 <- ggplot(data=df,aes(y=count,x="",fill= Living))+geom_bar(stat="identity",width = 0.5)+
  labs(title="Pie diagram to show Living area in the data
  '1' Represents Metroplitan area.
  '2' Represents Rural area.
  '3' Represents Small Town.",y="Frequency")
pie <- bar1 + coord_polar("y",start=0)
pie + geom_text(label=paste(df$count),position=position_stack(vjust = 0.5))

##Gender based on different age interval using multiple bar chart

df <-data%>%
  group_by(Living_area,Gender,Age)
df$Age  
count <- c(count1 <- nrow(df[df$Age=="18-25" & df$Gender=="Male",]),
           count2 <- nrow(df[df$Age=="18-25" & df$Gender=="Female",]),
           count3 <- nrow(df[df$Age=="26-35" & df$Gender=="Male",]),
           count4 <- nrow(df[df$Age=="26-35" & df$Gender=="Female",]), 
           count5 <- nrow(df[df$Age=="36-45" & df$Gender=="Male",]),
           count6 <- nrow(df[df$Age=="36-45" & df$Gender=="Female",]),
           count7 <- nrow(df[df$Age=="45 above" & df$Gender=="Male",]),
           count8 <- nrow(df[df$Age=="45 above" & df$Gender=="Female",]),
           count9 <- nrow(df[df$Age=="0-17" & df$Gender=="Male",]),
           count10 <- nrow(df[df$Age=="0-17" & df$Gender=="Female",]))

df_gen <- data.frame(Age=c(rep("18-25",2),rep("26-35",2),rep("36-45",2),rep("45 above",2),rep("0-17",2)),
                     Gender = rep(c("Male","Female"),5),Count = count)
ggplot(data=df_gen,aes(x=Age,fill=Gender,y=Count))+geom_bar(position="dodge",stat="identity",size=0.8)+
  labs(y="Count",x="Age intervals",title="Dissection of the data in Age intervals",subtitle="Gender wise")


##Living_area wrt Gender and Age using multiple bar chart 

df1 <- df[df$Age=="18-25",]
df2 <- df1[df1$Gender=="Male",]
tab1 <- data.frame(table(df2$Living_area))
count1 <- nrow(tab1)
df3 <- df1[df1$Gender=="Female",]
tab2 <- data.frame(table(df3$Living_area))
count2 <- nrow(tab2)

df1 <- df[df$Age=="26-35",]
df2 <- df1[df1$Gender=="Male",]
tab3 <- data.frame(table(df2$Living_area))
count3 <- nrow(tab3)
df3 <- df1[df1$Gender=="Female",]
tab4 <- data.frame(table(df3$Living_area))
count4 <- nrow(tab4)

df1 <- df[df$Age=="36-45",]
df2 <- df1[df1$Gender=="Male",]
tab5 <- data.frame(table(df2$Living_area))
count5 <- nrow(tab5)
df3 <- df1[df1$Gender=="Female",]
tab6 <- data.frame(table(df3$Living_area))
count6 <- nrow(tab6)

df1 <- df[df$Age=="45 above",]
df2 <- df1[df1$Gender=="Male",]
tab7 <- data.frame(table(df2$Living_area))
count7 <- nrow(tab7)
df3 <- df1[df1$Gender=="Female",]
tab8 <- data.frame(table(df3$Living_area))
count8 <- nrow(tab8)

df1 <- df[df$Age=="0-17",]
df2 <- df1[df1$Gender=="Male",]
tab9 <- data.frame(table(df2$Living_area))
count9 <- nrow(tab9)
df3 <- df1[df1$Gender=="Female",]
tab10 <- data.frame(table(df3$Living_area))
count10 <- nrow(tab10)

bar_data <- data.frame(Age =c(rep("18-25",6),rep("26-35",4),rep("36-45",4),rep("45 above",3),rep("0-17",1)),Living_area=c(tab1$Var1,tab2$Var1,tab3$Var1,tab4$Var1,tab5$Var1,tab6$Var1,tab7$Var1,tab8$Var1,tab9$Var1,tab10$Var1),
                       Gender=c(rep("Male",count1),rep("Female",count2),rep("Male",count3),rep("Female",count4),rep("Male",count5),rep("Female",count6),rep("Male",count7),rep("Female",count8),rep("Male",count9),rep("Female",count10)),
                       value=c(tab1$Freq,tab2$Freq,tab3$Freq,tab4$Freq,tab5$Freq,tab6$Freq,tab7$Freq,tab8$Freq,tab10$Freq)) 

bar_data
ggplot(data=bar_data,aes(x=Age,fill=Living_area,y=value))+geom_bar(position="dodge",stat="identity",size=0.8)+facet_wrap(~Gender)+
  labs(y="Count",x="Age intervals",title="Gender wise Living_area data",
       subtitle="Age interval wise
1 denotes Metropolitan cities
2 denotes Rural area
3 denotes Small towns")
