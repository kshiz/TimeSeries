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
ts_gdp = ts(data$Real.GDP....basis.2011.year., start = c(1800, 1))
ts_co2_mt = ts(data$CO2.in.metric.tons., start = c(1800, 1))
ts_co2_mil_mt = ts(data$CO2..in.million.metric.tons., start = c(1820, 1))
ts_pop = ts(data$Population, start = c(1800, 1))
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

combined_ts=cbind(ts_gdp,ts_co2_mt,ts_pop)

#gdp and co2
autoplot(combined_ts[,c('ts_gdp','ts_co2_mt')],facets = TRUE)+
  ggtitle('US CO2 emission vs Population')+xlab('Year')+ylab('')
#co2 and pop
autoplot(combined_ts[,c('ts_pop','ts_co2_mt')],facets = TRUE)+
  ggtitle('US CO2 emission vs Population')+xlab('Year')+ylab('GDP')
#Scatter plot
qplot(ts_co2_mt,ts_pop,data = as.data.frame(combined_ts))+ylab('CO2 emission in mt')+xlab("Population")
#training and test data
train_df=window(combined_ts,start=1800,end=1974)
test_df=window(combined_ts,start=1975,end=2018)
#classical decomposition
d=decompose(ts_co2_mt)
autoplot(d)+xlab('Year')+ggtitle('Classic Decomposition')
#x11(Not working)
library(seasonal)
x=seas(ts_gdp, start = c(1901, 1), x11 = "standard")

##Forecasting using decomposition
fit <- stl(ts_gdp, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot()  +
  ggtitle("Naive forecasts of seasonally adjusted data")