###########################################################################
## Exploration Script #2: YELP is planning a series of community nights where like-minded users can 
##  come in socialize at a restaurant that obviously is rated high by them. Write a code that takes 
##  in two user ids and returns their match score.
##  Logic: 
##  1. YELP selects a business group (aka. cluster 1/2/3/4)
##  2. YELP finds all users who visited the businesses in this cluster
##  3. Assume user rating is solely dependent on business category
##  4. Run a regression of stars given against business category PCA values.  
##  5. 
## Set the working directory, Change it to your location
setwd("C:/Avishek/Booth/Autumn 2017/37304 Digital And Algorithmic Marketing/YELP/JSON/Dataset")
##
###########################

source("Data_prep_transform.R")

review_all <-read.csv("review_all.csv")
View(head(review_all))

library(arules) 
write.csv(review_city, "review_city.csv")
businesstrans <- read.transactions(
  file = "review_city.csv",
  format = "single",
  sep = ",",
  cols=c("user_id","business_id"),
  rm.duplicates = T
)
summary(businesstrans)
## now apply the actual 'apriori' algorithm
# only rules with support > .001 & confidence >.5
yelprules <- apriori(businesstrans,parameter=list(support=.0002, confidence=.4)) 
## take a look
inspect(yelprules)

#business[which(business$business_id=='9PIC380tmNYpIC3z5cvcNA'),]

library(arulesViz)
plot(yelprules)
plot(subset(yelprules),method='grouped')
plot(subset(yelprules),method='graph')
