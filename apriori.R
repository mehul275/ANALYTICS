library(arules)

data=read.csv("Market_Basket_Optimisation.csv",header=F)
View(data)

matrix=read.transactions("Market_Basket_Optimisation.csv",sep = ',',rm.duplicates = T)
  #gives a sparse matrix which would be used in finding rules/combinations
  #separated by ,  removing duplicate values in a single transaction

#plotting items w.r.t. their frequencies
itemFrequencyPlot(matrix,topN=12)  

#applying apriori algo
rules=apriori(data=matrix,parameter = list(support=0.003,confidence=0.6))
   #rules are combination of correlated items in itemset

#visualizing rules in sorted manner
r=inspect(sort(rules,decreasing=T))
