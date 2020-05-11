#datasets are same as list but 2D
empdata=data.frame(emp_id=c(1:5),
                   emp_name=c("ram","shyam","ryan","ganj","dan"),
                   emp_sal=c(86,57,87,54,99))
print(empdata)
str(empdata)       #structure of dataframe no of obs; datatypes
summary(empdata)   #min, 1st q, median, mean, 3rd q, max
print(data.frame(empdata$emp_id,empdata$emp_name[4]))  #extracting data through columns
print(empdata[,3])         # by col
print(empdata[2,])         # by row
print(empdata[2,3])        # both 
print(empdata[1:2,])       #row 1 and 2
print(empdata[-c(1:2),])   #excluding them
print(empdata[c(5,3),c(2,3)])  

empdata$emp_dept=c("IT","ACCOUNTING","MARKETING","HR","ANALYTICS")
print(empdata)


#BINDING DATAFRAMES
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  dept = c("IT","Operations","IT","HR","Finance"),
  
)
emp.newdata <- 	data.frame(
  emp_id = c (6:8), 
  emp_name = c("Rasmi","Pranab","Tusar"),
  salary = c(578.0,722.5,632.8), 
  start_date = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  dept = c("IT","Operations","Fianance"),
  
)
# Bind the two data frames.
emp.finaldata <- rbind(emp.data,emp.newdata)
print(emp.finaldata)


#IMPORTING DATA FROM DATASET MTCARS
dataset=datasets::mtcars   #importing mtcars module from datasets
View(dataset)              #it will view dataset on r studio
edit(dataset)              #it will view dataset on r editor
head(dataset)              #it will slice 1st six elements
tail(dataset)              #it will slice last five elements

dataset[1,]
dataset[,]
dataset[,3]
dataset[2,4]

rownames(dataset)   #it can print rownames

# FIND THE CAR NAMES HAVING cyl==4

dataset$cyl==4    #it will match each and every element and return true/false

#  OR

dataset['cyl']==4    #accessing values of cyl column by either entering col name
dataset[2]==4        # or index of column

dataset[dataset['cyl'] == 4] #printing the whole corresonding row but printing col wise

#  0R

print(subset(dataset,dataset$cyl==4))  #printing the whole corresponding rows

#pre defined functions
sort(data$sal)
order(data$sal)




















