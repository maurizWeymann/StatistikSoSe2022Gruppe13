#Quellen: https://www.youtube.com/watch?v=_V8eKsto3Ug
#prozent anzeigen: https://stackoverflow.com/questions/3695497/show-percent-instead-of-counts-in-charts-of-categorical-variables
#prozent anzeigen2: https://stackoverflow.com/questions/40249943/adding-percentage-labels-to-a-bar-chart-in-ggplot2
#prozent in plot zeigen: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/
#Oskars Working Drive
setwd("C:/Users/Oskar/OneDrive/4 Semester HTW/Statistik/Hausuafagbe mit git/StatistikSoSe2022Gruppe13")
#Mauriz Working Drive
#setwd("G:/Meine Ablage/Sem 5/Statistik/TeamWork")
data<-read.csv("titanic_data.csv")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,tidyverse, scales,plyr) 

titanic_data <- data.table::fread("titanic_data.csv")
titanic_data <- titanic_data %>%  transmute(
  Survived =  factor(Survived, 
                     levels = c(0,1), 
                     labels = c("Died","Survived") 
  ),
  Class = factor(Pclass), 
  Sex = factor(Sex),
  Age = as.integer(Age),
  Siblings = SibSp,
  Parch,
  Fare = round(Fare,2),
  Cabin = substr( gsub("[^a-zA-Z]", "", Cabin), 1, 1),
  Embarked = factor(Embarked,
                    levels = c("C","Q","S"),
                    labels = c("Cherbourg","Queenstown","Southampton"))
)
titanic_data[titanic_data == ""] <- NA  

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
ggplot(data, aes(x = Survived, fill = Survived,label = scales::percent(prop.table(stat(count))))) + 
  geom_bar()+
  geom_text(stat = 'count',
            vjust = -.3) + 
  labs(y = "Number of Passengers",
       title = "Survival rate")+
  guides(fill="none")

#Tabelle
(survivalrate <- addmargins(table(data$Survived)))
(prop_survivalrate <- round(addmargins(prop.table(table(data$Survived))), 4) * 100)

#Survival rate vs class
ggplot(data, aes(x = Survived,group = Pclass,label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position="dodge",aes(fill=  factor(..x..))) +
  geom_text(stat = 'count',
            vjust = -.5) + 
  facet_grid(~Pclass) +
  labs(y = "Number of Passengers",
       x = "Passenger class",
       title = "Survival rate vs Class")+
  guides(fill="none")

#Relativ
ggplot(data, aes(x= Survived, group=Pclass)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Survived?",title = "Survival rate vs class") +
  facet_grid(~Pclass) +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")

#Tabelle
(s_vs_class <- addmargins(table(data$Survived,data$Pclass)))
(prop_s_vs_class <- round(addmargins(prop.table(table(data$Survived,data$Pclass))), 4) * 100)

#Survival rate vs Sex
ggplot(data, aes(x = Survived,group=Sex,label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge",aes(fill=  factor(..x..))) +
  geom_text(stat = 'count',
           vjust = -.5) + 
  facet_grid(~Sex) +
  labs(y = "Number of Passengers",
       title = "Survival vs Sex")+
  guides(fill="none")
#Relativ
ggplot(data, aes(x= Survived, group=Sex)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Survived?",title = "Survival vs Sex") +
  facet_grid(~Sex) +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")
#Tabelle
(s_vs_sex <- addmargins(table(data$Survived,data$Sex)))
(prop_s_vs_sex <- round(addmargins(prop.table(table(data$Survived,data$Sex))), 4) * 100)

#Survival rate vs age
ggplot(data, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  labs(y = "Number of Passengers",
       x = "Age (binwidth = 10)",
       title = "Survival vs Age")

ggplot(data, aes(x = Age, fill = Survived)) +
  geom_density(alpha= 0.7) +
  labs(y = "Number of Passengers",
       x = "Age",
       title = "Survival vs Age")

ggplot(data, aes(x = Survived, y = Age)) +
  geom_boxplot() +
  labs(y = "Age",
       x = "Survived",
       title = "Survival vs Age")
#Tabelle
(s_vs_age <- addmargins(table(data$Survived,data$Age)))
(prop_s_vs_age <- round(addmargins(prop.table(table(data$Survived,data$Age))), 4) * 100)

#Survival rate vs Number of Siblings/Spouses Aboard
ggplot(data, aes(x = SibSp, fill = Survived)) +
  geom_bar()+
  labs(y = "Number of Passengers",
       x = "Number of Siblings/Spouses Aboard",
       title = "Survival vs number of siblings/spouses aboard")
#Relativ
ggplot(data, aes(x= Survived, group=SibSp)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Survived?",title = "Survival vs number of siblings/spouses aboard") +
  facet_grid(~SibSp) +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")

#Tabelle
(s_vs_sibsp <- addmargins(table(data$Survived,data$SibSp)))
(prop_s_vs_sibsp <- round(addmargins(prop.table(table(data$Survived,data$SibSp))), 4) * 100)

#Survival rate vs Number of Parents/Children Aboard
#Analyse: 1-2 Parents/Children Aboard hat höhere überlebenschance 3-6 sind zu wenig daten
ggplot(data, aes(x = Parch, fill = Survived)) +
  geom_bar(position = "dodge")+
  labs(y = "Number of Passengers",
       x = "Number of Parents/Children Aboard",
       title = "Survival vs number of parents/children aboard")
#Relativ
ggplot(data, aes(x= Survived, group=Parch)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent",title = "Survival vs number of parents/children aboard") +
  facet_grid(~Parch) +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")

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
ggplot(data %>% drop_na(), aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(y = "Number of Passengers",
       x = "Port of embarkation",
       title = "Survival vs port of embarkation")
#Relativ
ggplot(data %>% drop_na(), aes(x= Survived, group=Embarked)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent",title = "Survival rate vs port of embarkation") +
  facet_grid(~Embarked) +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")
#Tabelle
(s_vs_embarked <- addmargins(table(data$Survived,data$Embarked)))
(prop_s_vs_embarked <- round(addmargins(prop.table(table(data$Survived,data$Embarked))), 4) * 100)
#Survival Rates vs Sex,Fare and Class
ggplot(data,aes(x=Pclass,y=Fare,fill= Survived))+
  geom_boxplot()+
  facet_grid(Sex ~ .)+
  ylim(0,180)+
  labs(x = "Passenger class",
       title = "Survival Rates vs Sex, Fare and Class")
#Survival Rates vs Sex, Age and Class
ggplot(data,aes(x=Pclass,y=Age,fill= Survived))+
  geom_boxplot()+
  facet_grid(Sex ~ .)+
  ylim(0,80)+
  labs(x = "Passenger class",
       title = "Survival Rates vs Sex, Age, Fare and Class")

#Junge männer zwischen 20-30 sterben mehr
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
#für Cabin mit anfangsbuchstaben bewerten
#Kontingenztafel machen mit abh?ngitkeiten vs unabh?ngigkeit
sex_impact<-count(data,Survived,Sex)
#Tabelle class,sex,embarked,Survived
(s_vs_age_test <- addmargins(table(data$Survived,data$Pclass,data$Sex)))
(prop_s_vs_age_test <- round(addmargins(prop.table(table(data$Survived,data$Pclass,data$Sex))), 4) * 100)

#Survival rate vs Cabin
ggplot(titanic_data %>% drop_na(), aes(x = Cabin, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(y = "Number of Passengers",
       x = "Cabin",
       title = "Survival vs cabin")
#Relativ
ggplot(titanic_data %>% drop_na(), aes(x= Survived, group=Cabin)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent",title = "Survival rate vs port of embarkation") +
  facet_grid(~Cabin) +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")
#tabelle
(s_vs_cabin <- addmargins(table(titanic_data$Survived,titanic_data$Cabin)))
(prop_s_vs_cabin <- round(addmargins(prop.table(table(titanic_data$Survived,titanic_data$Cabin))), 4) * 100)
#Cabin


(s_vs_test <- addmargins(table(titanic_data$Survived,titanic_data$Sex,titanic_data$Class)))
#Cabin,Sex,class
ggplot(titanic_data %>% drop_na(), aes(x = Cabin, fill = Survived)) +
  geom_bar(position = "dodge") +
  facet_grid(Class ~Sex)+
  labs(y = "Number of Passengers",
       x = "Cabin",
       title = "Survival Rates vs Cabin")
#relativ, Sex,class
ggplot(titanic_data, aes(x = Survived,group = Class,label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position="dodge",aes(fill=  factor(..x..))) +
  geom_text(stat = 'count',
            vjust = -.5) + 
  facet_grid(Class~Sex) +
  labs(y = "Number of Passengers",
       x = "Passenger class",
       title = "Survival rate vs Class")

#Survival rate vs class
ggplot(titanic_data, aes(x = Survived,group = Pclass,label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position="dodge",aes(fill=  factor(..x..))) +
  geom_text(stat = 'count',
            vjust = -.5) + 
  facet_grid(~Pclass) +
  labs(y = "Number of Passengers",
       x = "Passenger class",
       title = "Survival rate vs Class")+
  guides(fill="none")
