## CALLING THE DATA

rm(list=ls())
dataog <- read.csv(file = "Vaccination.csv",header = TRUE)
head(dataog)

## CALLING THE LIBRARIES
library(dplyr)
library(ggplot2)

##PREPARING THE DATA TO ANALYZE
## Columns to be modified--- Living area, Vac_safe, Vac_status, Vac_reliable, CoWin, Vac_symptom   
## Changing the column names to something short for future use.
colnames(dataog) <- c("TimeStamp","Age","Gender","Living_area","Vac_safe","Vac_status","Travel","1st_choice",
                    "2nd_choice","3rd_choice","4th_choice","5th_choice","6th_choice","Vac_brand",
                    "Vac_reliable","CoWin","Vac_app_easy","waiting_time","Social_dis","Hygiene",
                    "Vac_symptom","Govt_effort")

data <- dataog
data$Living_area <- as.numeric(as.factor(data$Living_area))
table (data$Living_area) 
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
df <- data.frame(table(data$Age))
bar <- ggplot(data=df,aes(y=Freq,x=Var1),beside=T)+geom_bar(stat="identity",width = 0.5)+
  labs(title="Age composition",x="Age Intervals",y="Frequency")+ylim(0,110)
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
df2 <- data.frame(table(data$Living_area))
bar <- ggplot(data=df2,aes(y= Freq,x= Var1),beside=T)+geom_bar(stat="identity",width = .75)+
  labs(title="Living area",x="Factor",y="Count")+
  annotate("text", x=1.19, y=60, label= "'1' Represents Metroplitan area.",size=3.5)+
  annotate("text", x=1.11, y=57, label= "'2' Represents Rural area.",size=3.5)+
  annotate("text", x=1.13, y=54, label= "'3' Represents Small Town.",size=3.5)
bar

bar1 <- ggplot(data=df2,aes(y=Freq ,x="",fill= Var1))+geom_bar(stat="identity",width = 0.5)+
  labs(title="Pie diagram to show Living area in the data
  '1' Represents Metroplitan area.
  '2' Represents Rural area.
  '3' Represents Small Town.",y="Frequency")
pie <- bar1 + coord_polar("y",start=0)
pie + geom_text(label=paste(df2$Freq),position=position_stack(vjust = 0.5))



##Living_area wrt Gender and Age using multiple bar chart 
names(data)
df <-data%>%
  group_by(Living_area,Gender,Age)
df$Age  

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
ggplot(data=bar_data,aes(x=Age,fill=Living_area,y=value))+
  geom_bar(position="dodge",stat="identity",size=0.8)+facet_wrap(~Gender)+
  labs(y="Number",x="Age intervals",title="Gender wise Living area data",
       subtitle="Age interval wise
1 denotes Metropolitan cities
2 denotes Rural area
3 denotes Small towns")


## EXPLORATORY DATA ANALYSIS
## 1.Vaccination safe wrt age
age_count <- data %>% 
  group_by(Age) %>% 
  summarise(count=n())
age_count
age_safe <- data %>% 
  group_by(Age,Vac_safe) %>%
  summarise(count=n())
age_safe
tab1 <- data.frame(table(age_safe$Age))
age_safe$tot_count <- rep(rep(age_count$count,times=tab1$Freq))  
age_safe$freq_den <- age_safe$count/age_safe$tot_count
ggplot(data=age_safe,aes(x=Age,y=freq_den,fill=as.factor(Vac_safe)),beside=TRUE)+geom_bar(position="dodge",stat="identity",width=0.5)+
  labs(title="Reliability on Vaccination",subtitle="  1 represents No, it can affect my health 
  2 represents Not Sure 
  3 is Yes",x="Age intervals",y="Frequency density",fill="Reliability on vaccine")


## 2. Vaccination safe wrt living area
living_count <- dataog%>%
  group_by(Living_area) %>% 
  summarise(count=n())
living_safe <- dataog %>% 
  group_by(Living_area,Vac_safe) %>% 
  summarise(count=n())  
living_safe
tab2 <- data.frame(table(living_safe$Living_area))
living_safe$count_tot <- rep(living_count$count,times=tab2$Freq)  
living_safe$freq_den <- living_safe$count/living_safe$count_tot
ggplot(data=living_safe,aes(x=as.factor(Living_area),y=freq_den,fill=as.factor(Vac_safe)))+
  geom_bar(position="dodge",stat="identity",width = 0.3)+
  labs(title="Reliability on Vaccination",x="Living areas",y="Frequency density",subtitle="Living area wise",fill="Reliability on vaccine")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0))+coord_flip()


## 3. Vaccination safe wrt Gender
gen_count <- dataog %>%
  group_by(Gender) %>% 
  summarise(count=n())
gen_safe <- dataog %>% 
  group_by(Gender,Vac_safe) %>% 
  summarise(count=n())
tab3 <- data.frame(table(gen_safe$Gender))
gen_safe$count_tot <- rep(gen_count$count,times=tab3$Freq)  
gen_safe$freq_den <- gen_safe$count/gen_safe$count_tot
ggplot(data=gen_safe,aes(x=as.factor(Gender),y=freq_den,fill=as.factor(Vac_safe)))+
  geom_bar(position="dodge",stat="identity",width=0.3)+
  labs(title="Reliability on Vaccination",x="Gender",y="Frequency density",subtitle="Gender wise",fill="Reliability on vaccine")+
  coord_flip()


#4. FREQUENCY-density analysis of Vaccination status wrt  Age and gender
agestatus <- data.frame(data$Age, data$Vac_status, data$Gender)
tabage <- data.frame(table (agestatus))
colnames(tabage)<- c("Age","Vaccination_Status","Gender","Number")
agestatus1 <- split(tabage,tabage$Gender)
agestatus_male <- agestatus1$Male
agestatus_female <- agestatus1$Female

age_count <- data %>% 
  group_by(Age,Gender) %>% 
  summarise(count=n())
age_count
age_count_M <- age_count[age_count$Gender=="Male",]
age_counttemp <- data.frame(Age="0-17",Gender="Male",count= 0)
age_count_M <- rbind (age_counttemp,age_count_M)
age_count_M1 <- rep(age_count_M$count,3)

age_count_F <- age_count[age_count$Gender=="Female",]
age_count_F1 <- rep(age_count_F$count,3)

agestatus_male$freq <- agestatus_male$Number/age_count_M1
agestatus_male$freq <- replace (agestatus_male$freq,c(1,6,11),0)
agestatus_female$freq <- agestatus_female$Number/age_count_F1
agestatus_gender <- rbind(agestatus_male,agestatus_female)


ggplot(data=agestatus_gender,aes(x= Age,y= freq,fill= Vaccination_Status))+
  geom_bar(position="dodge",stat="identity",size=0.8)+ facet_wrap(~Gender)+
  labs(y="Frequency density",x="Age Groups",title="Vaccination status data", subtitle="Gender wise
1 represents First dose is taken 
2 denotes Both dose is taken
3 denotes Not Vaccinated")


#5. Living area vs vaccination status
data2 <- dataog
living <- data2 %>% 
  group_by(Vac_status,Living_area) %>% 
  summarise (count= n())
living$freq_den <- living$count/living_count$count
tab4<- data.frame(table(living$Living_area))
ggplot(data=living,aes(x= Living_area,y= freq_den,fill= Vac_status))+
  geom_bar(position="dodge",stat="identity",size=0.8,width=0.3)+
  labs(y="Frequency density",x="Living area",title="Vaccination status",subtitle="Living area wise")+ coord_flip()


#6. Reliability on vaccine provider vs Living area 
living1 <- dataog %>% 
  group_by(Vac_reliable,Living_area) %>% 
  summarise (total= n())
living1
living1$freq_density <- living1$total/living_count$count

ggplot(data=living1,aes(x= Living_area,y= freq_density,fill= Vac_reliable))+
  geom_bar(position="dodge",stat="identity",width =0.4)+ coord_flip()+
  labs(y="Frequency",x="Living Area",title="Reliability on vaccine provider",
       fill= "Vaccine Provider", subtitle = "Living area wise")

#7. Drawing an inference from the data about vaccine availability by plotting the 
# travel area and the living area
avail <- dataog %>% 
  group_by(Travel,Living_area) %>% 
  summarise (count= n())
avail$freq_density <- avail$count/living_count$count 
ggplot(data=avail,aes(x= Living_area,y= avail$freq_density,fill= Travel))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="Living Area",title="Travel to get the vaccine",
       fill= "Travel to get the vaccine", subtitle = "Living area wise")

## age wise vaccine symptoms.
symp <- dataog %>%
  group_by(Age,Vac_symptom) %>% 
  summarise(count = n())
age_count <- data %>% 
  group_by(Age) %>% 
  summarise(count=n())
tab5 <- data.frame(table(symp$Age))
symp$freq_tot <- rep(age_count$count,times=tab5$Freq) 
symp$freq_den <- symp$count/symp$freq_tot
ggplot(data=symp ,aes(x= Age,y= freq_den ,fill= Vac_symptom ))+
  geom_bar(position="dodge",stat="identity",width =0.3)+
  labs(y="Count",x="Age group",title="Vaccine symptoms",
       fill= "Vaccine symptoms", subtitle = "Age wise",fill="Frequency density")



