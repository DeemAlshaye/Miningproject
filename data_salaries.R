#missing values
sum(is.na(ds_salaries))

#raw data sambles

ds_salaries[1:10,]
ds_salaries[100:110,]
ds_salaries[1000:1010,]
#statical mesurment
summary(ds_salaries$salary)

summary(ds_salaries$salary_in_usd)
summary(ds_salaries$remote_ratio)

#graphs

#Histogram

#Barchart

#Boxplot



#finding outliars
boxplot.stats(ds_salaries$salary)$out
boxplot.stats(ds_salaries$salary_in_usd)$out              
boxplot.stats(ds_salaries$remote_ratio)$out
boxplot.stats(ds_salaries$work_year)$out
#sum Outliars
sum(boxplot.stats(ds_salaries$salary)$out)

sum(boxplot.stats(ds_salaries$salary_in_usd)$out)

sum(boxplot.stats(ds_salaries$remote_ratio)$out)

sum(boxplot.stats(ds_salaries$work_year)$out)
#data cleaning

#checking for missing values
dim(ds_salaries)

ds_salaries=na.omit(ds_salaries)
dim(ds_salaries)

sum(is.na(ds_salaries))

#removing the outliers
library(outliers)
outliers <- boxplot(ds_salaries$salary, plot=FALSE)$out
ds_salaries <- ds_salaries[-which(ds_salaries$salary%in% outliers),]
boxplot.stats(ds_salaries$salary)$out

outliers <- boxplot(ds_salaries$salary_in_usd, plot=FALSE)$out
ds_salaries <- ds_salaries[-which(ds_salaries$salary_in_usd%in% outliers),]
boxplot.stats(ds_salaries$salary_in_usd)$out

outliers <- boxplot(ds_salaries$work_year, plot=FALSE)$out
ds_salaries <- ds_salaries[-which(ds_salaries$work_year%in% outliers),]
boxplot.stats(ds_salaries$work_year)$out

#normlize the data

ds_salaries$salary<-normalize(ds_salaries$salary)
ds_salaries$salary_in_usd<-normalize(ds_salaries$salary_in_usd)

#cor between salary and salary in usd
cor(ds_salaries$salary,ds_salaries$salary_in_usd)


