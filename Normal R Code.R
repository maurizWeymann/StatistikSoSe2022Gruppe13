#Quellen: https://www.youtube.com/watch?v=_V8eKsto3Ug
#prozent anzeigen: https://stackoverflow.com/questions/3695497/show-percent-instead-of-counts-in-charts-of-categorical-variables
#prozent anzeigen2: https://stackoverflow.com/questions/40249943/adding-percentage-labels-to-a-bar-chart-in-ggplot2

#Oskars Working Drive
#setwd("C:/Users/Oskar/OneDrive/4 Semester HTW/Statistik/Hausaufagbe v2")
#Mauriz Working Drive
setwd("G:/Meine Ablage/Sem 5/Statistik/TeamWork")
data<-read.csv("titanic_data.csv")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,tidyverse, scales) 

#Daten aufbereitung
data <-  data %>% mutate(Age = replace(Age, Age>0 & Age<1, NA))#Statt Na 0?
data$Age <- as.integer(data$Age)
data$Survived <- factor(data$Survived,
                        levels = c(0,1),
                        labels = c("Died","Survived"))
data$Pclass <- factor(data$Pclass)
data$Sex<- factor(data$Sex)
data$Embarked <- factor(data$Embarked,
                        levels = c("C","Q","S"),
                        labels = c("Cherbourg","Queenstown","Southampton"))
#Survival rate
ggplot(data, aes(x = Survived)) + 
  geom_bar()+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(y = "Number of Passengers",
       title = "Survival Rates")
#Tabelle
(survivalrate <- table(data$Survived))
#Survival rate vs class
ggplot(data, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  labs(y = "Number of Passengers",
       x = "Passenger class",
       title = "Survival Rates vs Class")

#Tabelle
(s_vs_class <- addmargins(table(data$Survived,data$Pclass)))
(prop_s_vs_class <- round(addmargins(prop.table(table(data$Survived,data$Pclass))), 4) * 100)
#Survival rate vs Sex
ggplot(data, aes(x = Sex, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
  geom_bar() +
  geom_text(stat = 'count',
            vjust = -0.4, 
            size = 3) + 
  labs(y = "Number of Passengers",
       title = "Survival Rates vs Sex")
#Tabelle
(s_vs_sex <- addmargins(table(data$Survived,data$Sex)))
(prop_s_vs_sex <- round(addmargins(prop.table(table(data$Survived,data$Sex))), 4) * 100)
#Survival rate vs age
ggplot(data, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  labs(y = "Number of Passengers",
       x = "Age (binwidth = 10)",
       title = "Survival Rates vs Age")

ggplot(data, aes(x = Age, fill = Survived)) +
  geom_density(alpha= 0.7) +
  labs(y = "Number of Passengers",
       x = "Age",
       title = "Survival Rates vs Age")

ggplot(data, aes(x = Survived, y = Age)) +
  geom_boxplot() +
  labs(y = "Age",
       x = "Survived",
       title = "Survival Rates vs Age")
#Tabelle
(s_vs_age <- addmargins(table(data$Survived,data$Age)))
(prop_s_vs_age <- round(addmargins(prop.table(table(data$Survived,data$Age))), 4) * 100)
#Survival rate vs Number of Siblings/Spouses Aboard
ggplot(data, aes(x = SibSp, fill = Survived)) +
  geom_bar()+
  labs(y = "Number of Passengers",
       x = "Number of Siblings/Spouses Aboard",
       title = "Survival Rates vs Number of Siblings/Spouses Aboard")
#Tabelle
(s_vs_sibsp <- addmargins(table(data$Survived,data$SibSp)))
(prop_s_vs_sibsp <- round(addmargins(prop.table(table(data$Survived,data$SibSp))), 4) * 100)
#Survival rate vs Number of Parents/Children Aboard

ggplot(data, aes(x = Parch, fill = Survived)) +
  geom_bar()+
  labs(y = "Number of Passengers",
       x = "Number of Parents/Children Aboard",
       title = "Survival Rates vs Number of Siblings/Spouses Aboard")
#Mit prozent
ggplot(data, aes(x = Parch, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
  geom_bar()+
  geom_text(stat = 'count',
            vjust = -0.6, 
            size = 3) + 
  labs(y = "Number of Passengers",
       x = "Number of Parents/Children Aboard",
       title = "Survival Rates vs Number of Siblings/Spouses Aboard")
#Tabelle
(s_vs_parch <- addmargins(table(data$Survived,data$Parch)))
(prop_s_vs_parch <- round(addmargins(prop.table(table(data$Survived,data$Parch))), 4) * 100)
#Survival rate vs Fare
ggplot(data, aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  labs(y = "Number of Passengers",
       x = "Fare (binwidth = 10)",
       title = "Survival Rates vs Fare")

ggplot(data, aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  labs(y = "Number of Passengers",
       x = "Fare (binwidth = 10)",
       title = "Survival Rates vs Fare")+
  xlim(0,300)
#Survival rate vs Embarked
ggplot(data %>% drop_na(), aes(x = Embarked, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
  geom_bar() +
  geom_text(stat = 'count',
            vjust = -0.6, 
            size = 3) + 
  labs(y = "Number of Passengers",
       x = "Passenger class",
       title = "Survival Rates vs Class")
#Tabelle
(s_vs_embarked <- addmargins(table(data$Survived,data$Embarked)))
(prop_s_vs_embarked <- round(addmargins(prop.table(table(data$Survived,data$Embarked))), 4) * 100)
#
ggplot(data,aes(x=Pclass,y=Fare,fill= Survived))+
  geom_boxplot()+
  facet_grid(Sex ~ .)+
  ylim(0,180)+
  labs(x = "Passenger class",
       title = "Survival Rates vs Sex Age and Fare")

#
ggplot(data, aes(x = Age, fill = Survived)) +
  geom_density(alpha= 0.7) +
  facet_grid(Sex ~ .)+
  labs(y = "Number of Passengers",
       x = "Age",
       title = "Survival Rates vs Age and Sex")

ggplot(data, aes(x = Survived, y = Age)) +
  geom_boxplot() +
  labs(y = "Age",
       x = "Survived",
       title = "Survival Rates vs Age")
#f?r Cabin mit anfangsbuchstaben bewerten
#Kontingenztafel machen mit abh?ngitkeiten vs unabh?ngigkeit
sex_impact<-count(data,Survived,Sex)
#Tabelle class,sex,embarked,Survived
(s_vs_age_test <- addmargins(table(data$Survived,data$Pclass,data$Sex)))
(prop_s_vs_age_test <- round(addmargins(prop.table(table(data$Survived,data$Pclass,data$Sex))), 4) * 100)

