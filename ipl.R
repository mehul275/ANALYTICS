#IMPORTING FILE : MATCHES.CSV

x=read.csv("matches.csv")    #copied the file in working directory
View(x)
print(is.data.frame(x))      #checking it as a df or not
print(nrow(x))              #getting rows
print(ncol(x))              #getting cols
print(max(x$win_by_runs))   #finding max runs 
x['id']==146                #accessing id column and giving a condition              

y=subset(x,win_by_runs==max(x$win_by_runs))#printing rows having the maximum win_by_runs
y

print(subset(x,x$win_by_runs>3 & x$win_by_runs<5)) #changing the condition
print(subset(x,as.Date(date)>as.Date("15-04-20"))) # error !

write.csv(y,"z.csv")  #making a new csv file then appending some data
read.csv("z.csv")     #reading csv file through functions

#find max no of times who was made player of match

max(table(x$player_of_match)) #it will only print max value table() will count

sort(table(x$player_of_match),decreasing = T) [1] #it will give value with names

table(x$winner)     #table() counts the frequency  if data is in numeric form
max(table(x$winner))  # coverting data into numeric form then finding highest then max 
sort(table(x$winner),decreasing = T,) [1] #

print(factor(x$season))     #12 seasons
print(factor(x$winner))     #16 teams


y=x[order(x$season),]   #putting index no in row names, with all columns 
y
#Find winner of IPL season wise

y1 = table(y$season) #finding count of seasons an dlast count is the final match of the season
(y1)
y1[[i]]    #2008,2009 are the row names....
y2=0         

m=2007       #printing years corresponding to winners

for (i in 1:length(y1))  #loop will run till the last season : 2019
{
  m=m+1                   #starting from 2008 to 2019
  print(m)                    
  y2=sum(y1[[i]],y2)       #finding sum i.e., last match of every season
  print(y2)
  print(y$winner[y1[[i]]])   #printing winners of every season
}
