##### LOyality clusters


library(readr)
library(sqldf)
library(mice)
library(ggplot2)
library(plyr)
transactions_200607 <- read_csv("F:/...../...../data.csv", col_types = cols(BASKET_ID = col_skip(), PROD_CODE_10 = col_number(), PROD_CODE_20 = col_number(), PROD_CODE_30 = col_number(), PROD_CODE_40 = col_number(), STORE_CODE = col_number(),PROD_CODE = col_number(), STORE_REGION = col_number()))

#range(transactions_200607$SHOP_WEEK )
dh <- transactions_200607
sam <- dh

# finding levels price sensitivity
fdata = factor(sam$CUST_PRICE_SENSITIVITY)
sam$CUST_PRICE_SENSITIVITY = as.integer(factor(sam$CUST_PRICE_SENSITIVITY,labels=c(1,2,3,4)))

# shop hour to integer
sam$SHOP_HOUR <- as.integer(sam$SHOP_HOUR)

#finding levels cust life stage
fdata = factor(sam$CUST_LIFESTAGE)
sam$CUST_LIFESTAGE = as.integer(factor(sam$CUST_LIFESTAGE,labels=c(1,2,3,4,5,6)))

#finding levels basket size
fdata = factor(sam$BASKET_SIZE)
sam$BASKET_SIZE = as.integer(factor(sam$BASKET_SIZE,labels=c(1,2,3)))

#finding levels basket size
fdata = factor(sam$BASKET_PRICE_SENSITIVITY)
sam$BASKET_PRICE_SENSITIVITY = as.integer(factor(sam$BASKET_PRICE_SENSITIVITY,labels=c(1,2,3,4)))

#finding levels basket size
fdata = factor(sam$BASKET_TYPE)
sam$BASKET_TYPE = as.integer(factor(sam$BASKET_TYPE,labels=c(1,2,3,4)))

#finding levels basket size
fdata = factor(sam$BASKET_DOMINANT_MISSION)
sam$BASKET_DOMINANT_MISSION = as.integer(factor(sam$BASKET_DOMINANT_MISSION,labels=c(1,2,3,4,5)))


#finding levels basket size
fdata = factor(sam$BASKET_DOMINANT_MISSION)
sam$BASKET_DOMINANT_MISSION = as.integer(factor(sam$BASKET_DOMINANT_MISSION,labels=c(1,2,3,4,5)))

#finding levels basket size
fdata = factor(sam$STORE_FORMAT)
sam$STORE_FORMAT = as.integer(factor(sam$STORE_FORMAT,labels=c(1,2,3,4)))

customer <- sam$CUST_CODE
sam$CUST_CODE <- NULL

# using mice library for imputation

# table showing missing values
f <- md.pattern(sam)
sam_old <- sam
# visualization of missing data
library(VIM)
aggr_plot <- aggr(sam, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(sam), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#converting following columns to factor to use mice for imputation
sam$CUST_LIFESTAGE <- as.factor(sam$CUST_LIFESTAGE)
sam$CUST_PRICE_SENSITIVITY <- as.factor(sam$CUST_PRICE_SENSITIVITY)

# imputation using logistic regression
tempData <- mice(sam,m=2,maxit=1,meth='polyreg',seed=500)
summary(tempData)

#Viewing imputated data
View(head(complete(tempData,1)))
View(head(complete(tempData,2)))
write.csv(complete(tempData,1), "F:\\...\\..\\iputations1.csv")
write.csv(complete(tempData,2), "F:\\...\\..\\imputations2.csv")
sam <- complete(tempData,1)
sam1 <- complete(tempData,2)

# removing NAs from customer codes
cdata_complete <- which(complete.cases(sam))
sam <- sam[cdata_complete,]

# finding levels price sensitivity
fdata = factor(sam$CUST_PRICE_SENSITIVITY)
sam$CUST_PRICE_SENSITIVITY = as.integer(factor(sam$CUST_PRICE_SENSITIVITY,labels=c(1,2,3,4)))

# shop hour to integer
sam$SHOP_HOUR <- as.integer(sam$SHOP_HOUR)

#finding levels cust life stage
fdata = factor(sam$CUST_LIFESTAGE)
sam$CUST_LIFESTAGE = as.integer(factor(sam$CUST_LIFESTAGE,labels=c(1,2,3,4,5,6)))

#finding levels basket size
fdata = factor(sam$BASKET_SIZE)
sam$BASKET_SIZE = as.integer(factor(sam$BASKET_SIZE,labels=c(1,2,3)))

#finding levels basket size
fdata = factor(sam$BASKET_PRICE_SENSITIVITY)
sam$BASKET_PRICE_SENSITIVITY = as.integer(factor(sam$BASKET_PRICE_SENSITIVITY,labels=c(1,2,3,4)))

#finding levels basket size
fdata = factor(sam$BASKET_TYPE)
sam$BASKET_TYPE = as.integer(factor(sam$BASKET_TYPE,labels=c(1,2,3,4)))

#finding levels basket size
fdata = factor(sam$BASKET_DOMINANT_MISSION)
sam$BASKET_DOMINANT_MISSION = as.integer(factor(sam$BASKET_DOMINANT_MISSION,labels=c(1,2,3,4,5)))

#finding levels basket size
fdata = factor(sam$BASKET_DOMINANT_MISSION)
sam$BASKET_DOMINANT_MISSION = as.integer(factor(sam$BASKET_DOMINANT_MISSION,labels=c(1,2,3,4,5)))

#finding levels basket size
fdata = factor(sam$STORE_FORMAT)
sam$STORE_FORMAT = as.integer(factor(sam$STORE_FORMAT,labels=c(1,2,3,4)))
cdata_complete <- which(complete.cases(sam))
sam <- sam[cdata_complete,]

# making an empty data frame
dh_empty <- data.frame(
  SHOP_HOUR = numeric(0),
  QUANTITY = numeric(0),
  SPEND = numeric(0),
  CUST_CODE = numeric(0),
  CUST_LIFESTAGE = numeric(0),
  CUST_PRICE_SENSITIVITY = numeric(0),
  BASKET_PRICE_SENSITIVITY = numeric(0),
  BASKET_TYPE = numeric(0),
  BASKET_DOMINANT_MISSION = numeric(0),
  STORE_FORMAT = numeric(0),
  STORE_REGION = numeric(0),
  R = numeric(0),
  F = numeric(0),
  M = numeric(0))
dh_empty$CUST_CODE <- as.character(dh_empty$CUST_CODE)
# making a list of unique customer code
cust_list <- list(unique(sam$CUST_CODE))
#str(cust_list)
length_of_cust <- lengths(cust_list)

# There are multiple enteries for each customer 
# we are combining them here 
#( mean for spend, shop hour and quantity for rest of them mode is used)
for(i in 1:length_of_cust){
  X <- cust_list[[1]][i]
  print(i)
  v <- "select * from sam where CUST_CODE =" 
  Example1 = paste(v, "'" ) 
  Example2 = paste(Example1, as.character(X),sep ="")
  Example3 = paste(Example2, "'" , sep ="") 
  Example = paste(Example3,';')
  sam_cust <- sqldf(Example, verbose = TRUE)
  #assign(paste("dun", X, sep = '_'), sam_cust)
  i <- i +1
  shophour <- mean(sam_cust$SHOP_HOUR)
  quant <- mean(sam_cust$QUANTITY)
  spend <- mean(sam_cust$SPEND)
  cust <- X
  # mode
  print(X)
  cps <- tail(names(sort(table(sam_cust$CUST_PRICE_SENSITIVITY))), 1)
  bps <- tail(names(sort(table(sam_cust$BASKET_PRICE_SENSITIVITY))), 1)
  bt <- tail(names(sort(table(sam_cust$BASKET_TYPE))), 1)
  bdm <- tail(names(sort(table(sam_cust$BASKET_DOMINANT_MISSION))), 1)
  
  cl <- tail(names(sort(table(sam_cust$CUST_LIFESTAGE))), 1)
  
  sf <- tail(names(sort(table(sam_cust$STORE_FORMAT))), 1)
  sr <- tail(names(sort(table(sam_cust$STORE_REGION))), 1)
  R <- max(sam_cust$SHOP_DATE)
  F <- length(sam_cust$SHOP_DATE)
  M <- sum(sam_cust$SPEND)
  dh_empty[nrow(dh_empty)+1, ] <- c(shophour, quant,spend,cust,cps,bps,bt,bdm,sf,sr,cl,R,F,M)
}
write.csv(dh_empty, "F:\\..\\..\\dh_empty.csv")


# ploting spend
dh_empty$SPEND <- as.numeric(dh_empty$SPEND)
qplot(as.numeric(dh_empty$SPEND), geom="histogram")
qplot(as.numeric(dh_empty$SPEND),
      geom="histogram",
      bins = 60,
      fill=I("blue"), 
      col=I("red"), 
      alpha = 0.3,
      xlim=c(0,4))
range(dh_empty$SPEND)

# removing outliers on the basis of histogram
h <- which(dh_empty$SPEND >= 6)
nor <- dh_empty[-h,]
out <- dh_empty[h,]

nor_customer <- nor$CUST_CODE
nor$CUST_CODE <- NULL

# converting to numeric
nor$SPEND <- as.numeric(nor$SPEND)
nor$SHOP_HOUR <- as.numeric(nor$SHOP_HOUR)
nor$CUST_PRICE_SENSITIVITY <- as.numeric(nor$CUST_PRICE_SENSITIVITY)
nor$BASKET_PRICE_SENSITIVITY <- as.numeric(nor$BASKET_PRICE_SENSITIVITY)
nor$BASKET_TYPE <- as.numeric(nor$BASKET_TYPE)
nor$BASKET_DOMINANT_MISSION <- as.numeric(nor$BASKET_DOMINANT_MISSION)
nor$STORE_FORMAT <- as.numeric(nor$STORE_FORMAT)
nor$STORE_REGION <- as.numeric(nor$STORE_REGION)
nor$score <- as.numeric(nor$score)
nor$QUANTITY <- as.numeric(nor$QUANTITY)
nor$CUST_LIFESTAGE <- as.numeric(nor$CUST_LIFESTAGE)

############################# RFM  ###############################################

# R is in terms wo week. Here we haven't taken day into account yet.
# R ranges from 1 to 7 ( one being most recent and 7 being least recent).
# however we want magnitude of most recent number to be largest.
# so we are subtracting erver R value from maximum value of R (i.e 7) and adding 1 to it.
# so 1 -> 7
#    2-> 6.....
#    7-> 1
# to convert scale from 1-7  to 0-100 we took inverse and multipied by 100.

nor$std_r <- (as.numeric(nor$R))
maxr <- max(nor$std_r)
nor$std_r <- (maxr - nor$std_r) + 1
range(nor$std_r)
nor$std_r <- (1/nor$std_r)*100

# in order to convert scale in range 1 to 100 we we took percantage over range of  F values.
nor$std_f <- (as.numeric(nor$F))
range(nor$std_f)
nor$std_f <- (100 * nor$std_f)/(max(nor$std_f) - min(nor$std_f))
range(nor$std_f)

# in order to convert scale in range 1 to 100 we we took percantage over range of  M values.
nor$std_m <- (as.numeric(nor$M))
range(nor$std_m)
nor$std_m <- (100 * nor$std_m)/(max(nor$std_m) - min(nor$std_m))
range(nor$std_m)

# calculating RFM score. Here more weightage is givento MOnetary aspect.
# since we are raising M by 1.1 higher values will get more raised as compared to lower values.
nor$score <- (nor$std_m)^1.1 * (nor$std_f) * (nor$std_r)
f <- nor$std_f
r <- nor$std_r
m <- nor$std_m 
nor$std_f <- NULL
nor$std_r <- NULL
nor$std_m <- NULL
nor$R <- NULL
nor$F <- NULL
nor$M <- NULL

# pass only numeric data
# elbow curve
wss <- c(0)
for (ii in 1:10) wss[ii] <- sum(kmeans(nor, centers=ii, iter.max=500, nstart=25)$withinss)
par(mfrow=c(1,1))
plot(1:10, wss, type="b", main="Elbow Curve without outliers", xlab="Number of Clusters", ylab="Within groups sum of squares")


# kmeans cluster 3 as per elbow curve
fitkmeans3 <- kmeans(nor, 3,  iter.max=1000, nstart=25) # 4 cluster solution
nkmeans3 <- fitkmeans3$cluster
nor$customer <- nor_customer
nor$cluster <- nkmeans3

write.csv(nor, "F:\\..\\..\\..\\nor.csv")



# seperating each cluster
nor$R <- r
nor$F <- f
nor$M <- m

#Separating customers of each cluster
s <- which(nor$cluster == 1 )
d1_1 <- nor[s,]

s <- which(nor$cluster == 2 )
d1_2 <- nor[s,]

s <- which(nor$cluster == 3 )
d1_3 <- nor[s,]


# visualization an inference
plot(nor$SHOP_HOUR, nor$cluster)
plot(nor$SPEND, nor$cluster)
plot(nor$score, nor$cluster)
plot(r, nor$cluster)
plot(f, nor$cluster)
plot(m, nor$cluster)

hist(d1_1$M)
hist(d1_2$M)
hist(d1_3$M)

hist(d1_1$F)
hist(d1_2$F)
hist(d1_3$F)

hist(d1_1$R)
hist(d1_2$R)
hist(d1_3$R)

length(d1_1[,1])
length(d1_2[,1])
length(d1_3[,1])
length(d1_4[,1])
length(out[,1])
length(nor[,1])

aggregate(score ~ cluster, data = nor, mean )
aggregate(M ~ cluster, data = nor, mean )
aggregate(SPEND ~ cluster, data = nor, mean )
aggregate(R ~ cluster, data = nor, mean )
aggregate(F ~ cluster, data = nor, mean )

# we are treating Outliers of spend as seperate cluster as their spend is way more than rest of people
mean(out$SPEND)
# mean of outlier is 9.89 which is way more than of the other group.



#### Further we tried to find out properties of each cluster..... But need help in this regard.


count(d1_1$STORE_REGION)
count(d1_2$STORE_REGION)
count(d1_3$STORE_REGION)
count(d1_4$STORE_REGION)
hist(d1_1$STORE_REGION)
hist(d1_2$STORE_REGION)
hist(d1_3$STORE_REGION)
hist(d1_4$STORE_REGION)

count(d1_1$CUST_PRICE_SENSITIVITY)
count(d1_2$CUST_PRICE_SENSITIVITY)
count(d1_3$CUST_PRICE_SENSITIVITY)
count(d1_4$CUST_PRICE_SENSITIVITY)
hist(d1_1$CUST_PRICE_SENSITIVITY)
hist(d1_2$CUST_PRICE_SENSITIVITY)
hist(d1_3$CUST_PRICE_SENSITIVITY)
hist(d1_4$CUST_PRICE_SENSITIVITY)

count(d1_1$BASKET_PRICE_SENSITIVITY)
count(d1_2$BASKET_PRICE_SENSITIVITY)
count(d1_3$BASKET_PRICE_SENSITIVITY)
count(d1_4$BASKET_PRICE_SENSITIVITY)
hist(d1_1$BASKET_PRICE_SENSITIVITY)
hist(d1_2$BASKET_PRICE_SENSITIVITY)
hist(d1_3$BASKET_PRICE_SENSITIVITY)
hist(d1_4$BASKET_PRICE_SENSITIVITY)

count(d1_1$BASKET_TYPE)
count(d1_2$BASKET_TYPE)
count(d1_3$BASKET_TYPE)
count(d1_4$BASKET_TYPE)
hist(d1_1$BASKET_TYPE)
hist(d1_2$BASKET_TYPE)
hist(d1_3$BASKET_TYPE)
hist(d1_4$BASKET_TYPE)

count(d1_1$BASKET_DOMINANT_MISSION)
count(d1_2$BASKET_DOMINANT_MISSION)
count(d1_3$BASKET_DOMINANT_MISSION)
count(d1_4$BASKET_DOMINANT_MISSION)
hist(d1_1$BASKET_DOMINANT_MISSION)
hist(d1_2$BASKET_DOMINANT_MISSION)
hist(d1_3$BASKET_DOMINANT_MISSION)
hist(d1_4$BASKET_DOMINANT_MISSION)

count(d1_1$STORE_FORMAT)
count(d1_2$STORE_FORMAT)
count(d1_3$STORE_FORMAT)
count(d1_4$STORE_FORMAT)
hist(d1_1$STORE_FORMAT)
hist(d1_2$STORE_FORMAT)
hist(d1_3$STORE_FORMAT)
hist(d1_4$STORE_FORMAT)

count(d1_1$STORE_REGION)
count(d1_2$STORE_REGION)
count(d1_3$STORE_REGION)
count(d1_4$STORE_REGION)
hist(d1_1$STORE_REGION)
hist(d1_2$STORE_REGION)
hist(d1_3$STORE_REGION)
hist(d1_4$STORE_REGION)

count(d1_1$CUST_LIFESTAGE)
count(d1_2$CUST_LIFESTAGE)
count(d1_3$CUST_LIFESTAGE)
count(d1_4$CUST_LIFESTAGE)
hist(d1_1$CUST_LIFESTAGE)
hist(d1_2$CUST_LIFESTAGE)
hist(d1_3$CUST_LIFESTAGE)
hist(d1_4$CUST_LIFESTAGE)

hist(d1_1$SPEND)
hist(d1_2$SPEND)
hist(d1_3$SPEND)
hist(d1_4$SPEND)

######################################################################################
##################################  END  #############################################
