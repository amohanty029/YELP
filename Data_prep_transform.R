###########################################################################
## Data_prep_transform.R: This is a common script that does the following:
## 1. Loads csv data for businesses, users, and reviews
## 2. Finds the city with maximum businesses and creates working data frames for the city only
## 3. Filters businesses for restaurants and food joints only
## 4. Runs logistic PCA on business data 
## 5. Attempts k-means and clusters data based on business categories
##
###########################

## 1.	Get all businesses by cities in the YELP dataset and find the city with maximum businesses.
business <- read.csv("business.csv")
dim(business) # 156,639 rows with 15 columns
colnames(business)
View(head(business))

# Read users data
users <- read.csv("user.csv")
dim(users) # 1,183,362 rows with 22 columns
colnames(users)
View(head(users))

# Read reviews data
reviews <- read.csv("review.csv")
dim(reviews) # 4,736,897 rows with 3 columns
colnames(reviews)
View(head(reviews))

## a.	We find Las Vegas as that city. We will work off Las Vegas for rest of the process.
count_businesses_by_city<- table(business$city)
count_businesses_by_city <- count_businesses_by_city[order(count_businesses_by_city,decreasing = T)]
head(count_businesses_by_city,1) # 24,768 businesses in Las Vegas.

target_city <- "Las Vegas"
business_city <- business[which(business$city==target_city),]
dim(business_city) # So, we have 24,768 businesses in Las Vegas in this dataset. 

# 2.	Filter the dataset to consider only businesses that have either 'Restaurants' or 'Food' as one 
#     of their descriptive.
pattern <- 'Restaurants|Food'
index <- grep(pattern, business_city$categories, perl=T)

business_city_Restaurants <- business_city[index,]
dim(business_city_Restaurants) # So, we have 7,303 restaurant/food businesses in Las Vegas in this dataset

review_city <- reviews[which(reviews$business_id %in% business_city_Restaurants$business_id),1:3]
review_city$user_id <- factor(review_city$user_id) #Refatoring the user ids, otherwise levels are getting goofy
dim(review_city) # 603,401 rows with 3 columns
colnames(review_city)
View(head(review_city))

# 3.	Prepare a dataset which is a matrix that consists of business id as the row name and 
#     all possible attribute types as column names. For each business id, only the relevant columns 
#     will be marked as 1.
Business_categories <- read.csv("lv_business_categories.csv")
Business_categories$value <- 1
business_city_restaurant_categories <-
  Business_categories[which(Business_categories$business_id %in% business_city_Restaurants$business_id),] 
business_city_restaurant_categories$category <- factor(business_city_restaurant_categories$category)
#Quick check...
dim(business_city_restaurant_categories)
length(unique(business_city_restaurant_categories$category))
View(head(business_city_restaurant_categories))

# Now, we will try to pivot the data table, so that categories become columns.
# Idea is to split the data by each category. This gives a series of lists for each category
restaurant.by.category <- split(business_city_restaurant_categories, business_city_restaurant_categories$category)

#Quick check...
typeof(restaurant.by.category)
names(restaurant.by.category)

# Convert to a data frame. Note: I used unique on business_id. 
# Why? How else, can a business_id become one row!
restaurant.category.universe <- data.frame(business_id=sort(unique(business_city_restaurant_categories$business_id)))

# Then, I merged each list by category to store as a new column in the universe data frame.
for (i in seq(along=restaurant.by.category)) 
{ 
  restaurant.category.universe[[names(restaurant.by.category)[i]]] <- 
    merge(restaurant.by.category[[i]], restaurant.category.universe, by.x=1, by.y=1,all=T)$value 
}

#Quick check...
dim(restaurant.category.universe)
View(head(restaurant.category.universe))
#See we have one extra column that number of categories (445). What is it? business_id!
ncol(restaurant.category.universe)

# Clean up the missing values to be FALSE
restaurant.category.universe[is.na(restaurant.category.universe)] <- 0
restaurant.category.universe2 <- restaurant.category.universe[,-1] # remove business_id. We will make it row name
rownames(restaurant.category.universe2) <- restaurant.category.universe[,1]
#Quick check...
View(head(restaurant.category.universe2))

# 4.	Attempt logistic PCA (principal component analysis on binary data) to boil the columns into few principal components. 
#     Why PCA? So many categories. we need to condense.

library(logisticPCA)
library(rARPACK)

pcacategories<- logisticPCA(restaurant.category.universe2,k=3,m=4,partial_decomp=T,main_effects=F)
pcacategories$prop_deviance_expl # Explains 94.75% of the deviance. So three PCAs are good enough.

library(rgl)
comp <- data.frame(pcacategories$PCs[,1:3])
#Qucik check...
View(head(comp))
plot(comp, pch=16, col=rgb(0.3,0.3,0.5))
#The 3D plot...
plot3d(comp$X1, comp$X2, comp$X3,col=rgb(0.3,0.3,0.5))

# Determine number of clusters. We see elbowing around 4. So, k=4!
wss <- (nrow(restaurant.category.universe2)-1)*sum(apply(restaurant.category.universe2,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(restaurant.category.universe2,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# Apply k-means with k=4
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)
# The cool 3D plot
plot3d(comp$X1, comp$X2, comp$X3, col=k$clust)

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

# 5.	Run a K-means to identify 4 sub-groups. This is our broad "business category"
# First cluster
row.names(restaurant.category.universe2[k$clust==clust[1],])
# Second Cluster
row.names(restaurant.category.universe2[k$clust==clust[2],])
# Third Cluster
row.names(restaurant.category.universe2[k$clust==clust[3],])
# Fourth Cluster
row.names(restaurant.category.universe2[k$clust==clust[4],])