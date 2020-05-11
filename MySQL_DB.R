install.packages('RMySQL')
library(RMySQL)

connection = dbConnect(MySQL(), user = 'root', password = '',
                       dbname = 'onlineshopping', host = 'localhost')

dbListTables(connection)

res <- dbSendQuery(connection, statement = 'Select * FROM products')
print(res)
data <- fetch(res)
# print(data)
View(data)

# Where Clause
res <- dbSendQuery(connection, 
                   statement = "Select * FROM products WHERE p_price < 40000")
data <- fetch(res)
# print(data)
View(data)
# create Table
df <- mtcars
dbWriteTable(connection, 'mtcars', df, overwrite = T)

iris <- iris
dbWriteTable(connection, 'iris', iris, overwrite = T)

