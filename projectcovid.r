## CALLING THE DATA

rm(list=ls())
dataog <- read.csv(file = "Vaccination.csv",header = TRUE)
head(dataog)

## CALLING THE LIBRARIES
library(dplyr)
library(ggplot2)
library(webr)

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
## 1. Age, 2. Gender, 3. Living_area, 
#4. Living_area wrt Gender and Age using multiple bar chart , 5. Gender based on different age interval using donut plot

##1. Bar and pie diagram of age composition of the data
df <- data.frame(table(data$Age))##calling the age column from the original data frame
bar <- ggplot(data=df,aes(y=Freq,x=Var1),beside=T)+geom_bar(stat="identity",width = 0.5)+
  labs(title="Age composition",x="Age Intervals",y="Frequency")+ylim(0,110)
bar

bar1 <- ggplot(data=df,aes(y=Freq,x="",fill=Var1))+geom_bar(stat="identity",width = 0.5)+
  labs(title="Age composition",x="Age Intervals",y="Frequency")
pie <- bar1 + coord_polar("y",start=0)
pie + geom_text(label=paste(df$Freq),position=position_stack(vjust = 0.5))


##2. pie diagram for gender composition of the data
df1 <- data.frame(table(data$Gender))##calling the Gender column from the original data frame
bar1 <- ggplot (df1, aes(y = Freq,x = "", fill= Var1))+geom_bar(stat = "identity", width = 0.5)+
  labs(title = "Gender Composition", x= "Gender", y= "Frequency")
pie<- bar1+ coord_polar("y", start = 0)
pie + geom_text(label=paste(df1$Freq),position=position_stack(vjust = 0.5))


##3. Bar and pie diagram of to categorize the living area of the data
table(data$Living_area)
df2 <- data.frame(table(data$Living_area)) ##calling the Living_area column from the original data frame
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


##4. Living_area wrt Gender and Age using multiple bar chart 
demo4 <- dataog %>% ## Grouping Gender, age and Living_area from the data 
  group_by(Age,Living_area,Gender) %>% 
  summarise(count= n())
tab00 <- data.frame(table (demo4$Age))
demo4$tot <- rep (age_count$count,times= tab00$Freq)
demo4$freq_density <- demo4$count/demo4$tot
##plotting the data in facet
ggplot(data=demo4,aes(x=Age,fill=Living_area,y=freq_density))+
  geom_bar(position="dodge",stat="identity",size=0.8)+facet_wrap(~Gender)+
  labs(y="Number",x="Age intervals",title="Gender wise Living area data",
       subtitle="Age interval wise
1 denotes Metropolitan cities
2 denotes Rural area
3 denotes Small towns")


#5. Gender based on different age interval using multiple bar chart
age_gender <- dataog %>% ## Grouping Gender and age from the data 
  group_by(Age,Gender) %>%
  summarise(count =n())
age_count <- data %>% ## counting the total number of data point in every age group
  group_by(Age) %>% 
  summarise(count=n())
tab0 <- data.frame(table (age_gender$Age))
age_gender$tot <- rep (age_count$count,times= tab0$Freq)
age_gender$freq_density <- age_gender$count/age_gender$tot
##plotting the data using the PieDonut plot from the "webr" package in R
PieDonut(age_gender, aes(Age, Gender, count=freq_density),
         title = "Gender based on age interval",showRatioThreshold = F,donutLabelSize = 3)

## EXPLORATORY DATA ANALYSIS
## 1.Vaccination safe wrt age
age_count
age_safe <- data %>% 
  group_by(Age,Vac_safe) %>%
  summarise(count=n())
age_safe
tab1 <- data.frame(table(age_safe$Age))
age_safe$tot_count <- rep(rep(age_count$count,times=tab1$Freq))##total number of people in each age group 
age_safe$freq_den <- age_safe$count/age_safe$tot_count ##counting the frequency density for each age group
ggplot(data=age_safe,aes(x=Age,y=freq_den,fill=as.factor(Vac_safe)),beside=TRUE)+geom_bar(position="dodge",stat="identity",width=0.5)+
  labs(title="Reliability on Vaccination",subtitle="  1 represents No, it can affect my health 
  2 represents Not Sure 
  3 is Yes",x="Age intervals",y="Frequency density",fill="Reliability on vaccine")


## 2. Vaccination safe wrt living area
living_count <- dataog%>% ##counting the total number of people living in each Living areas
  group_by(Living_area) %>% 
  summarise(count=n())
living_safe <- dataog %>% ##grouping living area and belief towards vaccination and plotting them using barplot
  group_by(Living_area,Vac_safe) %>% 
  summarise(count=n())  
living_safe
tab2 <- data.frame(table(living_safe$Living_area))
living_safe$count_tot <- rep(living_count$count,times=tab2$Freq)#making a total column of the total number of data in 
#each data points
living_safe$freq_den <- living_safe$count/living_safe$count_tot #frequency density of the total number of people living 
# in different areas
##plotting the frequency density of the reliability on vaccine, gender wise
ggplot(data=living_safe,aes(x=as.factor(Living_area),y=freq_den,fill=as.factor(Vac_safe)))+
  geom_bar(position="dodge",stat="identity",width = 0.3)+
  labs(title="Reliability on Vaccination",x="Living areas",y="Frequency density",subtitle="Living area wise",fill="Reliability on vaccine")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0))+coord_flip()


## 3. Vaccination safe wrt Gender
gen_count <- dataog %>% ##counting the total number of female and male in our data
  group_by(Gender) %>% 
  summarise(count=n())
gen_safe <- dataog %>% ##grouping Gender and belief towards vaccination and plotting them using barplot
  group_by(Gender,Vac_safe) %>% 
  summarise(count=n())
tab3 <- data.frame(table(gen_safe$Gender))
gen_safe$count_tot <- rep(gen_count$count,times=tab3$Freq)## finding the total number of people that found vaccine safe
#gender wise
gen_safe$freq_den <- gen_safe$count/gen_safe$count_tot ##finding the frequency density
ggplot(data=gen_safe,aes(x=as.factor(Gender),y=freq_den,fill=as.factor(Vac_safe)))+
  geom_bar(position="dodge",stat="identity",width=0.3)+
  labs(title="Reliability on Vaccination",x="Gender",y="Frequency density",subtitle="Gender wise",fill="Reliability on vaccine")+
  coord_flip()


#4. FREQUENCY-density analysis of Vaccination status wrt  Age and gender
agestatus <- data.frame(data$Age, data$Vac_status, data$Gender)
#making a dataframe with only the age, vaccination status and the gender of the data
tabage <- data.frame(table (agestatus))
colnames(tabage)<- c("Age","Vaccination_Status","Gender","Number")
agestatus1 <- split(tabage,tabage$Gender)
agestatus_male <- agestatus1$Male
agestatus_female <- agestatus1$Female
## splitting the gender column in male and female and storing them in separate values

age_count <- data %>% #grouping the original data frame in Age and Gender
  group_by(Age,Gender) %>% 
  summarise(count=n())
age_count
age_count_M <- age_count[age_count$Gender=="Male",]
age_counttemp <- data.frame(Age="0-17",Gender="Male",count= 0)
age_count_M <- rbind (age_counttemp,age_count_M)
age_count_M1 <- rep(age_count_M$count,3)
##making the data suitable for plotting 


age_count_F <- age_count[age_count$Gender=="Female",]
age_count_F1 <- rep(age_count_F$count,3)
##finding the frequency data for male and the female separately
agestatus_male$freq <- agestatus_male$Number/age_count_M1
agestatus_male$freq <- replace (agestatus_male$freq,c(1,6,11),0)
agestatus_female$freq <- agestatus_female$Number/age_count_F1
agestatus_gender <- rbind(agestatus_male,agestatus_female)

##plotting the Vaccination status of male and female separately(age wise) 
ggplot(data=agestatus_gender,aes(x= Age,y= freq,fill= Vaccination_Status))+
  geom_bar(position="dodge",stat="identity",size=0.8)+ facet_wrap(~Gender)+
  labs(y="Frequency density",x="Age Groups",title="Vaccination status data", subtitle="Gender wise
1 represents First dose is taken 
2 denotes Both dose is taken
3 denotes Not Vaccinated")


#5. Living area vs vaccination status
data2 <- dataog
living <- data2 %>% ##grouping data according to their vaccination status and Living area
  group_by(Vac_status,Living_area) %>% 
  summarise (count= n())
living$freq_den <- living$count/living_count$count #making a new column to make the frequency density data
tab4<- data.frame(table(living$Living_area))
#plotting living area vs frequency density of the vaccination status of the people
ggplot(data=living,aes(x= Living_area,y= freq_den,fill= Vac_status))+
  geom_bar(position="dodge",stat="identity",size=0.8,width=0.3)+
  labs(y="Frequency density",x="Living area",title="Vaccination status",subtitle="Living area wise")+ coord_flip()


#6. Reliability on vaccine provider vs Living area 
living1 <- dataog %>% ##grouping Reliability towards vaccine and Living area of the individual
  group_by(Vac_reliable,Living_area) %>% 
  summarise (total= n())
living1
living1$freq_density <- living1$total/living_count$count
#plotting the living area vs the frequency of the number of people that find which vaccine more reliable
#(free or paid)
ggplot(data=living1,aes(x= Living_area,y= freq_density,fill= Vac_reliable))+
  geom_bar(position="dodge",stat="identity",width =0.4)+ coord_flip()+
  labs(y="Frequency",x="Living Area",title="Reliability on vaccine provider",
       fill= "Vaccine Provider", subtitle = "Living area wise")

#7. Drawing an inference from the data about vaccine availability by plotting the 
# travel area and the living area
avail <- dataog %>% ##travelling done by individual wrt to their living area
  group_by(Travel,Living_area) %>% 
  summarise (count= n())
avail$freq_density <- avail$count/living_count$count 
ggplot(data=avail,aes(x= Living_area,y= avail$freq_density,fill= Travel))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="Living Area",title="Travel to get the vaccine",
       fill= "Travel to get the vaccine", subtitle = "Living area wise")


##8. Cowin-use vs Age
cowin_age <- dataog %>% ##cowin app use wrt age groups
  group_by(Age,CoWin) %>% 
  summarise(count = n())
colnames(cowin_age) <- c("Age","Cowin","Count")
tab6 <- data.frame(table (cowin_age$Age))
cowin_age$freq <- rep(age_count$count,times=tab6$Freq)
cowin_age$freq_density <- cowin_age$Count/cowin_age$freq ## counting the frequency density for each age group

ggplot(data=cowin_age ,aes(x= Age,y= freq_density,fill= Cowin))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="Age group",title="Cowin usage",
       fill= "Cowin usage", subtitle = "Age wise")

##9. Cowin-use vs living area
cowin_living <- dataog %>% ##cowin app use wrt living area of the individual
  group_by(CoWin,Living_area) %>% 
  summarise(count = n())
cowin_living$freq_density <- cowin_living$count/living_count$count

ggplot(data=cowin_living ,aes(x= Living_area,y= freq_density,fill= CoWin))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="Living area",title="Cowin usage",
       fill= "Cowin usage", subtitle = "Living area wise")


#10. CoWin use >> vaccination appointment ease
f <- function(x)
{
  if(x==1)
    return("No")
  else
    return("Yes")
}
cowin_data <- data %>% 
  select(CoWin,Vac_app_easy)
## we originally had three option as response to this question-
# "No, I have not used the portal"
# "Yes, but I did not face any problem"
# "Yes, I faced a lot of issues while booking"
#and converting the responses into Yes or No on using the CoWin app or portal
cowin_data$CoWin <- sapply(cowin_data$CoWin,f)## converting the 3 choice from the original column 
##in the data to yes and no
cowin_data1 <- cowin_data %>% ## counting the number of people who used and didnt use Cowin app
  group_by(Vac_app_easy,CoWin) %>% 
  summarise(count=n())
cowin_data1

tab6 <- data.frame(table(cowin_data$CoWin))
cowin_data1$tot <- rep(tab6$Freq,times=5)
cowin_data1$freq_den <- cowin_data1$count/cowin_data1$tot

ggplot(data=cowin_data1,aes(x= CoWin,y= freq_den,fill= as.factor(Vac_app_easy)))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="CoWin portal used",title="Using CoWin portal vs ease in getting an appointment",
       fill= "", subtitle = "1 signifies least difficulty 
5 signifies maximum difficulty
(Data is in 1-5 likert scale)")


## 11. pie diagram of cowin usage
# Pie chart of the number of people using the oowin app or portal and 
# the number of people who faced a problem while using it
Cowinbar1 <- ggplot (cowin_data1, aes(y = count ,x = "", fill= CoWin))+geom_bar(stat = "identity", width = 0.5)+
  labs(title = "Percentage of Cowin usage", x= "", y= "")
cowinpie1 <- Cowinbar1+ coord_polar("y", start = 0)

cowinyes <- dataog[dataog$CoWin== "Yes, I faced a lot of issues while booking"| dataog$CoWin== "Yes, but I did not face any problem",]
cowinyes <- cowinyes %>% 
  group_by(CoWin) %>% 
  summarise(count = n())

Cowinbar2 <- ggplot (cowinyes, aes(y = count ,x = "", fill= CoWin))+geom_bar(stat = "identity", width = 0.5)+
  labs(title = "Problem faced while using Cowin", x= "", y= "")
cowinpie2 <- Cowinbar2 + coord_polar("y", start = 0)
cowinpie1 + geom_text(label= c(""),position=position_stack(vjust = 0.5))
cowinpie2 + geom_text(label= c(""),position=position_stack(vjust = 0.5))


#12. Multiple bar plot of first and second choice vaccine.
vaccine1 <- data.frame(table (dataog$`1st_choice`))
vaccine2 <- data.frame(table (dataog$`2nd_choice`))
vaccine3 <- data.frame(table (dataog$`3rd_choice`))
vaccine4 <- data.frame(table (dataog$`4th_choice`))
vaccine5 <- data.frame(table (dataog$`5th_choice`))
vaccine6 <- data.frame(table (dataog$`6th_choice`))
ggplot(data=vaccine1 ,aes(x= Var1,y= Freq ,fill= ))+
  geom_bar(position="dodge",stat="identity",fill = '#DF536B',col= "#2297E6",width =0.3)+
  labs(y="Count",x="Vaccine brand",title="#1 Vaccine choice")

ggplot(data=vaccine2 ,aes(x= Var1,y= Freq ,fill= ))+
  geom_bar(position="dodge",stat="identity",fill = '#DF536B',col= "#2297E6",width =0.3)+
  labs(y="Count",x="Vaccine brand",title="#2 Vaccine choice")

ggplot(data=vaccine3 ,aes(x= Var1,y= Freq ,fill= ))+
  geom_bar(position="dodge",stat="identity",fill = '#DF536B',col= "#2297E6",width =0.3)+
  labs(y="Count",x="Vaccine brand",title="#3 Vaccine choice")

ggplot(data=vaccine4 ,aes(x= Var1,y= Freq ,fill= ))+
  geom_bar(position="dodge",stat="identity",fill = '#DF536B',col= "#2297E6",width =0.3)+
  labs(y="Count",x="Vaccine brand",title="#4 Vaccine choice")

ggplot(data=vaccine5 ,aes(x= Var1,y= Freq ,fill= ))+
  geom_bar(position="dodge",stat="identity",fill = '#DF536B',col= "#2297E6",width =0.3)+
  labs(y="Count",x="Vaccine brand",title="#5 Vaccine choice")

ggplot(data=vaccine6 ,aes(x= Var1,y= Freq))+
  geom_bar(position="dodge",stat="identity",fill = '#DF536B',col= "#2297E6",width =0.3)+
  labs(y="Count",x="Vaccine brand",title="#6 Vaccine choice")


##13. Symptoms after vaccination>>vaccine brand wise
vac_data <- dataog %>% ##grouping by the vaccine symptoms and vaccine brand.
  group_by(Vac_brand,Vac_symptom)%>%
  summarise(count=n())
vac_data <- vac_data[-9,]
brand_count <- data.frame(table(dataog$Vac_brand))
brand_count <- brand_count[-3,]
vac_data$tot <- rep(brand_count$Freq,each=4)
vac_data$Freq_den <- vac_data$count/vac_data$tot #finding the frequency density for each vaccine brand 
ggplot(data=vac_data,aes(x= Vac_brand,y= Freq_den,fill= as.factor(Vac_symptom)))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="Vaccine brands",title="Symptoms after vaccination",
       subtitle = "Vaccine brand wise",fill= "Symptoms")+coord_flip()


##14. waiting time vs Living area for who has not traveled
travel_no <- dataog[dataog$Travel== "No",] ##selecting all the cases  for which there were no travels bu person
waiting <- travel_no %>% 
  group_by(Living_area,waiting_time) %>% 
  summarise(count= n())
living_count_no <- travel_no %>% 
  group_by(Living_area) %>% 
  summarise(count=n())
waiting$freq_density <- waiting$count/living_count_no$count
ggplot(data=waiting,aes(x= Living_area,y= freq_density,fill= waiting_time))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="Living area",
       title="Waiting time in queue",
       fill= "", subtitle ="Living area-wise" )


##15. Hygiene vs Living area for who has not traveled
travel_no <- dataog[dataog$Travel=="No",]
hygiene <- travel_no %>% ##comparing hygiene in different living area 
  group_by(Living_area,Hygiene) %>% 
  summarise(count=n())
living_count_no <- travel_no %>% 
  group_by(Living_area) %>%## counting the number of people who didnt travel, living area wise 
  summarise(count=n())
hygiene$tot <- rep(living_count_no$count,each=5)
hygiene$Freq_den <- hygiene$count/hygiene$tot
ggplot(data=hygiene,aes(x= Living_area,y= Freq_den,fill= as.factor(Hygiene)))+
  geom_bar(position="dodge",stat="identity",width =0.3)+
  labs(y="Frequency density",x="Living area",title="Hygiene maintenance",
       subtitle = "Vaccination place wise
1 denotes poorest hygiene maintenance
5 denotes best hygiene maintenance",
       fill= "Hygiene level")+coord_flip()


## 16. Social distancing vs living area
distancing <- travel_no %>% ##grouping social distancing with living area 
  group_by(Living_area,Social_dis) %>% 
  summarise(count=n())
tab16 <- data.frame(table (distancing$Living_area))
distancing$freq <- rep(living_count_no$count,times=tab16$Freq)
distancing$freq_density <- distancing$count/distancing$freq #calculating the freq density 
ggplot(data=distancing,aes(x= Living_area,y= freq_density,fill= as.factor(Social_dis)))+
  geom_bar(position="dodge",stat="identity",width =0.4)+
  labs(y="Frequency density",x="Living area",
       title="Social distancing maintained",
       fill= "Social distancing", subtitle ="Living area-wise
1 denotes no social distancing
5 denotes adequate social distancing" )


##17.age wise vaccine symptoms.
symp <- dataog %>% ##grouping age and vaccination symptoms of our data
  group_by(Age,Vac_symptom) %>% 
  summarise(count = n())
age_count <- dataog %>% 
  group_by(Age) %>% 
  summarise(count=n())
tab5 <- data.frame(table(symp$Age))
symp$freq_tot <- rep(age_count$count,times=tab5$Freq) 
symp$freq_den <- symp$count/symp$freq_tot
ggplot(data=symp ,aes(x= Age,y= freq_den ,fill= Vac_symptom ))+
  geom_bar(position="dodge",stat="identity",width =0.3)+
  labs(y="Frequency density",x="Age group",title="Vaccine symptoms",
       fill= "Vaccine symptoms", subtitle = "Age wise")


##18. age and gender wise vaccine symptoms.
gen <- split(dataog,data$Gender)
gen_male <- gen$Male
gen_female <- gen$Female

male_count <- gen_male %>% 
  group_by(Age) %>% 
  summarise(count=n())

female_count <- gen_female %>% 
  group_by(Age) %>% 
  summarise(count=n())

symp_male <- gen_male %>% 
  group_by(Age,Gender,Vac_symptom) %>% 
  summarise(count=n())
tab17 <- data.frame(table(symp_male$Age))
symp_male$tot <- rep(male_count$count,times=tab17$Freq)
symp_male$freq_den <- symp_male$count/symp_male$tot

symp_female <- gen_female %>% 
  group_by(Age,Gender,Vac_symptom) %>% 
  summarise(count=n())
tab18 <- data.frame(table(symp_female$Age))
symp_female$tot <- rep(female_count$count,times=tab18$Freq)
symp_female$freq_den <- symp_female$count/symp_female$tot

symp1 <- rbind(symp_male,symp_female)

ggplot(data=symp1 ,aes(x= Age,y= freq_den ,fill= as.factor(Vac_symptom)))+
  geom_bar(position="dodge",stat="identity",width =0.3)+ facet_wrap(~Gender)+
  labs(y="Count",x="Age group",title="Vaccine symptoms",
       fill= "Vaccine symptoms", subtitle = "Age and Gender wise")


##19. Age-wise categorization of 1st choice vaccines using PieDonut
age_vaccine <- dataog %>% 
  group_by(Age,`1st_choice`) %>% 
  summarise(count= n())
colnames(age_vaccine) <- c("Age","First_choice","Count")
tab19 <- data.frame(table (age_vaccine$Age))
age_vaccine$tot <- rep(age_count$count,times=tab19$Freq)
age_vaccine$freq_density <- age_vaccine$Count/age_vaccine$tot
ggplot(data=age_vaccine ,aes(x= Age,y=freq_density ,fill= First_choice ))+
  geom_bar(position="dodge",stat="identity",width =0.3)+
  labs(y="Frequency density",x="Age group",title="Vaccine symptoms",
       fill= "Vaccine symptoms", subtitle = "Age wise")
PieDonut(age_vaccine, aes(Age, First_choice, count=freq_density), title = "First choice vaccine age wise",showRatioThreshold = F,donutLabelSize = 2)

##20. pie diagram of governments effort.
govn <- data.frame(table(dataog$Govt_effort))##calling the Government effort from the original data frame
bar20 <- ggplot (govn, aes(y = Freq,x = "", fill= Var1))+geom_bar(stat = "identity", width = 0.5)+
  labs(title = "Government effort", x= "Rating", y= "Frequency",fill ="Government rating")
pie20 <- bar20+ coord_polar("y", start = 0)
pie20 + geom_text(label=paste(govn$Freq),position=position_stack(vjust = 0.5))



##21. Pie-Donut of Government effort, and categorizing it age wise
govn_age <- dataog %>%
  group_by(Age,Govt_effort) %>% 
  summarise(count = n())
tab21 <- data.frame(table(govn_age$Age))
govn_age$tot <- rep (age_count$count,times= tab21$Freq)
govn_age$freq_density <- govn_age$count/govn_age$tot
PieDonut(govn_age, aes(Age,Govt_effort, count=freq_density), title = "Government effort",
         showRatioThreshold = F,donutLabelSize = 2)


