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


#encoding
ds_salaries$company_size = factor(ds_salaries$company_size,levels = c("S","M","L"),labels = c(1,2,3))


#graphs:

#Histogram
hist(Salary,ylim=c(0,100))

#Bar plot
library(magrittr)

ds_salaries$job_title %>% table() %>% barplot(xlab="job title", ylab="number of employees", main="barplot of jobs")
ds_salaries$remote_ratio %>% table() %>% barplot(xlab="remote ratio", main="barplot of remote ratio")
ds_salaries$work_year %>% table() %>% barplot(xlab="work year", ylab="number of employees", main="barplot of work year")


#Boxplot
boxplot(remote_ratio~company_size, data=ds_salaries)
boxplot(salary~experience_level, data=ds_salaries)
boxplot(salary~employment_type, data=ds_salaries)
boxplot(salary~company_size, data=ds_salaries)
boxplot(salary~work_year, data=ds_salaries)
boxplot(work_year~experience_level, data=ds_salaries)
boxplot(company_size~employment_type, data=ds_salaries)

#scatter plot
with(ds_salaries,plot(company_size,salary,xlab="company_size", ylab="salary"))


#Pie chart
library(dplyr)
ds_salaries2 <- ds_salaries %>% sample_n(500)
ds_salaries2$experience_level %>% table() %>% pie() 
tab <- ds_salaries2$experience_level %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%')
pie(tab, labels=txt ,xlab="experience level") 


ds_salaries2 <- ds_salaries %>% sample_n(500)
ds_salaries2$company_size %>% table() %>% pie() 
tab <- ds_salaries2$company_size %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%')
pie(tab, labels=txt , xlab="company size")

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

#we normalized the attributes salary, salary in usd, so it takes values between 0 and 1 ,which helps in handling the data

#cor between salary and salary in usd
cor(ds_salaries$salary,ds_salaries$salary_in_usd)


