library(tidyverse)

reg<-as.tibble(read_csv("D5 SHSAT Registrations and Testers.csv"))

#What does the table initially look like?
head(reg)

#Finding the total number of students at each school, how many registered, and how many took the test
reg<-reg%>%
  group_by(`School name`)%>%
  mutate(enrollSum= sum(`Enrollment on 10/31`))%>%
  mutate(regSum= sum(`Number of students who registered for the SHSAT`))%>%
  mutate(tookSum= sum(`Number of students who took the SHSAT`))

#Finding the average number of students at each school per year, how many registered, and how many took the test
reg<-reg%>%
  group_by(`School name`)%>%
  mutate(enrollAvg= mean(`Enrollment on 10/31`))%>%
  mutate(regAvg= mean(`Number of students who registered for the SHSAT`))%>%
  mutate(tookAvg= mean(`Number of students who took the SHSAT`))


#Calculating percentages of those who registered of the total population, registered AND took the test, and those who took the test of the total population
reg<-reg%>%
  group_by(`School name`)%>%
  mutate(RegPct=regSum/enrollSum)%>%
  mutate(TookPct=tookSum/enrollSum)%>%
  mutate(TestYield=tookSum/regSum)


#Let's remove some columns that have no use for our future analysis.
reg<-reg%>%
  select(-DBN)%>%
  select(-`Year of SHST`)%>%
  select(-`Grade level`)

#What does the table currently look like?
head(reg)


#Let's clean up the other sheet
school<-as.tibble(read_csv("2016 School Explorer.csv"))

#what does the table look like?
head(school)

#remove unneccessary columns
school<-school%>%
  select(-`Adjusted Grade`)%>%
  select(-`New?`)%>%
  select(-`Other Location~)
  