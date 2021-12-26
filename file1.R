
library(scales)
library(tidyverse)
library(scales)
library(tidyverse)
library(hash)
library(dplyr)
data <- read.csv("C:\\Users\\HP\\Desktop\\Data Science Assignment\\DS_COVID_19_Analysis_R\\country_vaccinations.csv")
colnames(data)
#Getting useful columns
data_set<-data[,1:13]
#Putting 0 at NA values
data_set[is.na(data_set)==1]=0
#Summary of dataset
data_set_summary<-summary(data_set[,4:12])
data_set_summary
#Removing countries which make UK
countries_remove= c("England", "Scotland", "Wales", "Northern Ireland")
data_set <- data_set %>%
  filter (!country %in% countries_remove)
unique(data_set$country)
# Getting vaccines names
data_set$vaccines <- str_replace_all(data_set$vaccines, " ","")
vaccine_values<- unique(data_set$vaccines)
vaccines<- vector()
for (i in vaccine_values){
  for (j in strsplit(i, ",")){
    vaccines<- c(vaccines, j)
  }
}
vaccine_names<- unique(vaccines)
vaccine_names
#Making a new dataset and removing repetition of countries with respect to vaccines
data_set1<-data.frame(data_set$country,data_set$vaccines)
data_set1<-unique(data_set1)
#Making new column to count countries 
vaccines_count<-c(rep(0,length(vaccine_names)))
#Taking count of countries according to the vaccines used
for(i in 1:nrow(data_set1))
{
  x<-data_set1[i,2]
  for(j in strsplit(x,","))
  {
    index<-match(j, vaccine_names)
    val<-vaccines_count[index]
    vaccines_count[index]<-val+1
    
  }
}
#Plotting graph
#Note: In Python there was globe map representing countries with respect to vaccines. I plot a graph which represents count of countries based on vaccines.

ggplot(mapping=aes(x=vaccine_names, y=vaccines_count,fill = vaccine_names))+
  geom_col() +
  labs(x = "Vaccines", y = "No. of Countries", title = "Use of vaccines in countries")+
  geom_text(aes(label = vaccines_count), vjust=-2.0)+
  theme_minimal()



