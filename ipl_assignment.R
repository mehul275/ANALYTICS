# ASSIGMENT
# 1.Highest runs in one match/inning by a team

x=read.csv("deliveries.csv")
x
View(x)

x=x%>%group_by(batting_team,match_id)%>%  #grouping batting teams 
      summarise(Score=sum(total_runs))%>%  #then finding total runs scored by ba team
          arrange(desc(Score))  #arranging in desc order to find max
        
head(x,1)

#2. Highest runs in one inning by an individual player
x=read.csv("deliveries.csv")

x%>%group_by(batsman)%>%               #grouping batsman 
        summarise(Sc=sum(batsman_runs))%>% #then finding total runs scored by each batsman
          arrange(desc(Sc))               ##arranging in desc order to find max

head(x,1)  #highest scoring batsman

#3. Highest runs of top 5 players in ipl till now
x=read.csv("deliveries.csv")

x=x%>%group_by(batsman)%>%               #grouping batsman 
  summarise(Sc=sum(batsman_runs))%>% #then finding total runs scored by each batsman
  arrange(desc(Sc))

head(x,5)   # top 5 highest scoring batsman

#4. Highest wicket taker in IPL

x=read.csv("deliveries.csv")

x=x%>%group_by(bowler)%>%               #grouping bowlers
  summarise(Score=sum(dismissal_kind=='bowled' | dismissal_kind=='lbw'))%>% #then finding total wickets taken by each bowler
  arrange(desc(Score))   #max wicket taker

head(x,1) #highest wicket taker 

#5 max 6 runs
library(dplyr)

dataset1 <- read.csv('deliveries.csv')
View(dataset)

temp_dataset = dataset1%>%
  select(batsman,batsman_runs)%>%
  filter(batsman_runs==6)

temp_dataset
final=head(temp_dataset%>%group_by(batsman))
final[1,1]

######OR######

x=read.csv("deliveries.csv")

x=x%>%group_by(batsman)%>%
    summarise(Score=sum(batsman_runs==6)/6)%>%
        arrange(desc(Score))
x



















