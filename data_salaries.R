#missing values, we used is.null function to show any null values in the dataset, we did not have any.
sum(is.na(ds_salaries))

#raw data sambles
#sample of 10 rows of our raw data 

set.seed(10)
n<-sample(1:nrow(ds_salaries),10)
ds_salaries[n, ]

ds_salaries[1:10,]
ds_salaries[100:110,]
ds_salaries[1000:1010,]
#statical mesurment
summary(ds_salaries$salary)

summary(ds_salaries$salary_in_usd)
summary(ds_salaries$remote_ratio)


#encoding
ds_salaries$company_size = factor(ds_salaries$company_size,levels = c("S","M","L"),labels = c(1,2,3))
ds_salaries$experience_level = factor(ds_salaries$experience_level,levels = c("EN","MI","SE","EX"),labels = c(1,2,3,4))

#graphs:we compared some variables with each other to find how they are distributed

#Histogram
hist(Salary,ylim=c(0,100))
#the histogram represent the amout of salaries for each employee, it shows that most of employees has small amount of salary 

#Bar plot
library(magrittr)

ds_salaries$job_title %>% table() %>% barplot(xlab="job title", ylab="number of employees", main="barplot of jobs")
ds_salaries$remote_ratio %>% table() %>% barplot(xlab="remote ratio", main="barplot of remote ratio")
#the bar plot represent the total of remote ratio for each employee, it show that 0(onsite) is the most remote ratio
ds_salaries$work_year %>% table() %>% barplot(xlab="work year", ylab="number of employees", main="barplot of work year")
#the bar plot represent the work year and number of employee in each year, it show that 2020 year has the lowest number of employees
#the bar plot of 'Top 5 job Title Salaries' represent the job title and the salary for each job, it shows that Head of Machine Learning is the highest salary

#Boxplot
boxplot(remote_ratio~company_size, data=ds_salaries)
boxplot(salary~experience_level, data=ds_salaries)
boxplot(salary~employment_type, data=ds_salaries)
boxplot(salary~company_size, data=ds_salaries)
boxplot(salary~work_year, data=ds_salaries)
boxplot(work_year~experience_level, data=ds_salaries)
boxplot(company_size~employment_type, data=ds_salaries)

#scatter plot
with(ds_salaries,plot(salar_in_usd,salary,xlab="company_size", ylab="salary"))


#Pie chart
library(dplyr)
ds_salaries2 <- ds_salaries %>% sample_n(500)
ds_salaries2$experience_level %>% table() %>% pie() 
tab <- ds_salaries2$experience_level %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%')
pie(tab, labels=txt ,xlab="experience level") 
#the pie chart represent the percentages for experience level by taken sample of employees, it shows that SE(Senior level) has the highest frequency 


ds_salaries2 <- ds_salaries %>% sample_n(500)
ds_salaries2$company_size %>% table() %>% pie() 
tab <- ds_salaries2$company_size %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%')
pie(tab, labels=txt , xlab="company size")
#the pie chart represent the percentages for company size by taken sample of company, it shows that 2(Medium) has the highest frequency 



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
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}
ds_salaries$salary<-normalize(ds_salaries$salary)
ds_salaries$salary_in_usd<-normalize(ds_salaries$salary_in_usd)

#we normalized the attributes salary, salary in usd, so it takes values between 0 and 1 ,which helps in handling the data

#cor between salary and salary in usd
cor(ds_salaries$salary,ds_salaries$salary_in_usd)
