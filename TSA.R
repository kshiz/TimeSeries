#installing the necessary library
install.packages('tseries') 
#loading the libraray
library(tseries)
#reading the dataset
data=read.csv("USA GDP CO2.csv")
#structure of data
str(data)
#converting year column to date format
data$year = as.Date(paste(data$year, "-01-01", sep=""))
# Create ts objects for GDP and CO2
ts_gdp = ts(data$Real.GDP....basis.2011.year., start = c(1800, 1), frequency = 1)
ts_co2_mt = ts(data$CO2.in.metric.tons., start = c(1800, 1), frequency = 1)
ts_co2_mil_mt = ts(data$CO2..in.million.metric.tons., start = c(1820, 1), frequency = 1)
ts_pop = ts(data$Population, start = c(1800, 1), frequency = 1)
#plotting the ts objects
install.packages("ggplot2")
library('ggplot2')
install.packages('ggfortify')
library(ggfortify)
#gdp
autoplot(ts_gdp)+
  ggtitle('US GDP')+xlab('Year')+ylab('GDP')
#co2
autoplot(ts_co2_mt)+
  ggtitle('US CO2 emission')+xlab('Year')+ylab('GDP')
#population
autoplot(ts_pop)+
  ggtitle('US Population')+xlab('Year')+ylab('GDP')

df=ts(data[,3:7],start=c(1800,1),frequency=1)

#gdp and co2
autoplot(df[,c('Real.GDP....basis.2011.year.','CO2.in.metric.tons.')],facets = TRUE)+
  ggtitle('US CO2 emission vs Population')+xlab('Year')+ylab('')
#co2 and pop
autoplot(df[,c('Population','CO2.in.metric.tons.')],facets = TRUE)+
  ggtitle('US CO2 emission vs Population')+xlab('Year')+ylab('GDP')
#Scatter plot
qplot(Population,CO2.in.metric.tons.,data = as.data.frame(df))+ylab('CO2 emission in mt')+xlab("Population")