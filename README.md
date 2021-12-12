# Topics-in-Business-Analytics-Mini-Project-Airbnb
Analyzing the profiles of Airbnb listings in New York City
##Next we read the data from the working directory
airbnb_nyc_data <- read.csv("AB_NYC_2019.csv")
View(airbnb_nyc_data)
#install.packages("writexl")
require(writexl)
require(dplyr)
require(psych)
str(airbnb_nyc_data)
dim(airbnb_nyc_data)
summary(airbnb_nyc_data)
View(describe(airbnb_nyc_data))

###EDA of the variables
unique(airbnb_nyc_data$id)
library(ggplot2)
ggplot(airdatanum2, aes(price)) + geom_histogram(binwidth = 5)
ggplot(airbnb_nyc_data, aes(minimum_nights)) + geom_histogram(binwidth = 100)
colnames(airbnb_nyc_data)
airbnbfactorvars <- c('neighbourhood_group', 'neighbourhood', 'room_type')
airbnbnumvars <- c( 'price', 'minimum_nights','number_of_reviews',
                  'reviews_per_month', 'calculated_host_listings_count', 'availability_365')
###Cleaning and formating of the variables
airbnbcatvars <- apply(airbnb_nyc_data[airbnbfactorvars], 2, FUN = as.factor)
airbnbcatvars <- data.frame(airbnbcatvars)
View(airbnbcatvars)
airbnbnumbers <- apply(airbnb_nyc_data[airbnbnumvars], 2, FUN = as.numeric)
airbnbnumbers <- data.frame(airbnbnumbers)
View(airbnbnumbers)
airbnbdata <- cbind(airbnbcatvars, airbnbnumbers)
View(airbnbdata)
airbnbdata['last_review'] <- airbnb_nyc_data$last_review
str(airbnbdata)
as.POSIXct( airbnbdata['last_review'][[1]], format = "%d-%m-%Y", tz = "UCT")
airbnbdata['last_review'] <- as.POSIXct( airbnbdata['last_review'][[1]], format = "%d-%m-%Y", tz = "UCT")
str(airbnbdata)
airbnbdata['id'] = airbnb_nyc_data$id
airbnbdata['host_id'] = airbnb_nyc_data$host_id
airbnbdata['name'] = airbnb_nyc_data$name
airbnbdata['host_name'] = airbnb_nyc_data$host_name
airbnbdata['latitude'] = airbnb_nyc_data$latitude
airbnbdata['longitude'] = airbnb_nyc_data$longitude
dates <- airbnbdata[['last_review']]
class(dates)
dates[1]
###Let us write a function to get the recency variable
recency <- function(x){rec <- c()
for (i in c(1:length(x))){
 rec[i] = (as.numeric(as.POSIXct(Sys.Date(), format = "%Y-%m-%d") ) - as.numeric(x[i]))/(24*60*60) 
}
  return(rec)}
recency(dates)
length(dates)
airbnbdata['days_since_last_review'] <- recency(dates)
View(airbnbdata)
str(airbnbdata)
dim(airbnbdata)
##Using a user-defined function to understand the missings and the outliers of the dataset
num_vars <- sapply(airbnbdata, is.numeric)
numdatasumm <- function(x){n = length(x) 
nmiss = sum(is.na(x)| (x == "" )| x == "#NULL!" ) 
min = min(x, na.rm = TRUE)
max = max(x, na.rm = TRUE)
mean = mean(x, na.rm  = TRUE)
s = sd(x, na.rm = TRUE)
pctl = quantile(x, na.rm = TRUE, p = c(0.01, 0.25, 0.50, 0.75, 0.99) )
skewness = skew(x, na.rm = TRUE)
UC = mean + 3*s
LC = mean - 3*s
return(c(n = n, nmiss = nmiss, min= min, max = max, mean = mean, stddev = s,
         skew = skewness,pctl = pctl, uc = UC, lc = LC ))
}
numvariablesummary <- t(apply(airbnbdata[num_vars],2, FUN = numdatasumm ))
View(numvariablesummary)
numvariablesummary <- data.frame(numvariablesummary)
#write_xlsx(numvariablesummary1, "numvarsum12.xlsx")
##Converting the categorical variables into factors
airbnbdata$neighbourhood <- as.factor(airbnbdata$neighbourhood)
airbnbdata$neighbourhood_group <- as.factor(airbnbdata$neighbourhood_group)
airbnbdata$room_type <- as.factor(airbnbdata$room_type)
cat_vars <- sapply(airbnbdata, is.factor)
catvarsumm <- function(x) { 
  n = length(x) 
  nmiss = sum(is.na(x)|(x == "")| (x == "#NULL!") ) 
  return(c( n = n, nmiss = nmiss ))}
catsumm <- t(apply(airbnbdata[cat_vars], 2, catvarsumm ))
View(catsumm)
### Thus we see from the above summary of numeric variables and categorical variables that 
### the missing values are there in only 'last_review' and 'recency_of_review' variables
##Next we treat the outliers of the numeric continuous variables using a user defined function
uppercaptreat <- function(x) { x <- replace(x, which(x > quantile(x, probs = c(0.99), na.rm = TRUE)), quantile(x, probs = c(0.99), na.rm = TRUE)) }
lowercaptreat <- function(x) { x <- replace(x, which(x < quantile(x, probs = c(0.01), na.rm = TRUE)), quantile(x, probs = c(0.01), na.rm = TRUE)) }
colnames(airbnbdata)
str(airbnbdata)
airdatanum1 <- apply(airbnbdata[c('price', 'minimum_nights','number_of_reviews','reviews_per_month','calculated_host_listings_count',
                                  'availability_365', 'days_since_last_review','latitude','longitude')],2,uppercaptreat)
airdatanum1 <- data.frame(airdatanum1)
airdatanum2 <- apply(airdatanum1,2,lowercaptreat)
airdatanum2 <- data.frame(airdatanum2)
str(airdatanum2)
##Next we treat the missing values of custdatacont1 data set using a user defined function
missimputation <- function(x){ x <- replace (x, which(is.na(x) == TRUE | x == "#NULL!"), mean(x, na.rm = TRUE))}

airdatanum2 <- apply(airdatanum2, 2, missimputation)
airdatanum2 <- data.frame(airdatanum2)
str(airdatanum2)
View(describe(airdatanum2))
descairdata <- describe(airdatanum2)
#write_xlsx(descairdata, 'airdatatreated.xlsx')
##Next we create a  user defined function to impute the missings or the #NULL! values of categorical variables
catmissimpute <- function(x){ x <- replace(x, which(x == "#NULL!"), as.numeric(levels(x)[which(prop.table(table(x,useNA = "no"  )) == max(prop.table(table(x, useNA = "no"))))]))}
###Since we do not have any missing value in the categorical variables we don't use this function now
corrm1 <- cor(airdatanum2)
View(corrm1)
##Next we present the average of logtotalspent and totalspent against predlogtoalspent and predtotalspent in a table in groups of different deciles using sql
require(sqldf)
install.packages("GPArotation")
library(GPArotation)
corrm <- cor(airdatanum2)
View(corrm)
corrm <- data.frame(corrm)
#write_xlsx(corrm,"corrm.xlsx")
eigenvalues <- mutate(data.frame(eigen(corrm)$values), cum_sum_eigen=cumsum(eigen.corrm..values), pct_var=eigen.corrm..values/sum(eigen.corrm..values), cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))
View(eigenvalues)
#write_xlsx(eigenvalues, 'eigenvalues.xlsx')
###After viewing the eigen values we see that we get 7 factors to understand more than 85% of the dataset
require(psych)
scree(corrm, factors = TRUE, pc = T, hline = NULL, add = FALSE)
FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")
print(FA)
FA_sort <- fa.sort(FA)
Fac_load <- data.frame(FA_sort$loadings[1:9,])
View(Fac_load)
#write_xlsx(Fac_load,"Factor_Loadings.xlsx")
### Trying cluster analysis on the data
airdatastd1 <- scale(airdatanum2[,c(1,2,3,4,5,6)])
View(airdatastd1)

##K-means proceedure 
cluster3latest <- kmeans( airdatastd1, 3 )
cluster4 <- kmeans( airdatastd, 4 )
cluster5 <- kmeans( airdatastd, 5 )
cluster6 <- kmeans( airdatastd, 6 )
##Next we get the full dataset with the cluster numbers stated
airdatafinal12 <- cbind(airdatanum2, clus_3 = cluster3$cluster, clus_4 = cluster4$cluster, clus_5 = cluster5$cluster,  clus_6 = cluster6$cluster)
airdatafinal12 <- cbind(airdatanum2, clus_3 = cluster3latest$cluster)

View(airdatafinal12)
airdatafinal12$clus_3 <- as.factor(airdatafinal$clus_3)
airdatafinal12$clus_4 <- as.factor(airdatafinal$clus_4)
airdatafinal12$clus_5 <- as.factor(airdatafinal$clus_5)
airdatafinal12$clus_6 <- as.factor(airdatafinal$clus_6)
#install.packages('tables')
require(tables)
#write_xlsx(airdatafinal12,"airdatafianl12.xlsx")
profile33 <- tabular(1 + price +  minimum_nights + number_of_reviews + reviews_per_month + calculated_host_listings_count + availability_365 
                   ~ mean + mean*clus_3, data = airdatafinal12  )
profile <- data.frame(as.matrix(profile3))
View(profile)
View(airdatafinal1)
#write_xlsx(profile, "airbnbclusters1.xlsx")
airdatafinal12$id = airbnb_nyc_data$id
airdatafinal12['host_id'] <- airbnb_nyc_data$host_id
airdatafinal12$name = airbnb_nyc_data$name
airdatafinal12$host_name = airbnb_nyc_data$host_name
airdatafinal12['neighbourhood'] <- airbnb_nyc_data$neighbourhood
airdatafinal12['neighborhood_group'] <- airbnb_nyc_data$neighbourhood_group
airdatafinal12['room_type'] <- airbnb_nyc_data$room_type
#install.packages("factoextra")
#install.packages("ggpubr")
#install.packages("car")
require(car)
require(factoextra)
?fviz_cluster
cluster6
fviz_cluster(cluster3latest, data = airdatastd1, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
vif(fit1)
