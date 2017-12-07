###########################################################################
## Exploration Script #1: Write a code that finds potential users who are likely to rate it 4 or 
# more stars for a given business id.

## Set the working directory, Change it to your location
setwd("C:/Avishek/Booth/Autumn 2017/37304 Digital And Algorithmic Marketing/YELP/JSON/Dataset")

###########################
source("Data_prep_transform.R")

# 6.	Exercise #1: Write a code that finds potential users who are likely to rate it 4 
#     or more stars for a given business id.
selected_bus_id <- 'bqDFZDfDPdCut0CbybDUrA' # try with different business_ids (from Las Vegas only)

# a.	Find the cluster in which the business belongs.
for (i in c(1:4))
{
  if(selected_bus_id %in% row.names(restaurant.category.universe2[k$clust==clust[i],]))
  {
    print(paste("found '", selected_bus_id,"' in cluster number: ", i, sep = ""))
    break;
  }
}

# b.	Get all users for the business who have rated it 4 or more stars. This is our seed.
seed <- unique(review_city$user_id[which(review_city$business_id == selected_bus_id & review_city$stars>=4)])
length(seed) 

#Quick check...
View(review_city[which(review_city$business_id == selected_bus_id & review_city$stars>=4),])

# c.	Now, get all users who have visited businesses belonging to the same cluster, but have not 
#     visited this business. This will be our target.
target <- unique(review_city$user_id[which(review_city$stars>=4 & review_city$business_id %in% row.names(restaurant.category.universe2[k$clust==clust[i],]))])
length(target) 
target <- target[which(!('%in%'(target,seed)))]
length(target) # Should be length(all_user_ids) - length(seed)

# d.	Run a KNN between target and seed to find potential new customers.

seed.df <- users[which(users$user_id %in% seed),c("user_id","review_count","useful","funny","cool","fans","average_stars"
            ,"compliment_hot","compliment_more","compliment_profile","compliment_cute","compliment_list"
            ,"compliment_note","compliment_plain","compliment_cool","compliment_funny","compliment_writer","compliment_photos")]
dim(seed.df)
View(head(seed.df))

target.df <- users[which(users$user_id %in% target),c("user_id","review_count","useful","funny","cool","fans","average_stars"
                                                  ,"compliment_hot","compliment_more","compliment_profile","compliment_cute","compliment_list"
                                                  ,"compliment_note","compliment_plain","compliment_cool","compliment_funny","compliment_writer","compliment_photos")]
dim(target.df)
View(head(target.df))

seed_mm=model.matrix(~review_count+useful+funny+cool+fans+average_stars+compliment_hot+
                    compliment_more+compliment_profile+compliment_cute+compliment_list+compliment_note+
                    compliment_plain+compliment_cool+compliment_funny+compliment_writer+compliment_photos-1, data=seed.df)

target_mm=model.matrix(~review_count+useful+funny+cool+fans+average_stars+compliment_hot+
                    compliment_more+compliment_profile+compliment_cute+compliment_list+compliment_note+
                    compliment_plain+compliment_cool+compliment_funny+compliment_writer+compliment_photos-1, data=target.df)
library(FNN)

#Assumption: If it takes $1 to acquire a customer, the benefit of him/her rating the business 1 star is $.24.   

per_star_dollar_value <- 0.24
cost <- 1
profit=rep(0,10)

for(k in 1:10){
  mk = get.knnx(target_mm,seed_mm,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit[k] = per_star_dollar_value*sum(target.df[matches,"average_stars"])-cost*length(target.df[matches,"average_stars"]) 
}
# Plot
plot(profit,type='b',pch=19)
abline(v=8,lty=2)

# Max profits
k.star = which.max(profit); k.star
mk.star = get.knnx(target_mm,seed_mm,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit.star = per_star_dollar_value*sum(target.df[matches,"average_stars"])-cost*length(target.df[matches,"average_stars"]) #Team check this
profit.star

# So, who are the users that this business should target?...
length(target.df[matches,1]) 
target.df[matches,1]

business[which(business$business_id=='bqDFZDfDPdCut0CbybDUrA'),]
