airbnb <- read.csv("AB_NYC_2019.csv")

#Clean data
airbnb$id <- NULL
airbnb$name <- NULL
airbnb$host_id <- NULL
airbnb$host_name <- NULL
airbnb$latitude <- NULL
airbnb$longitude <- NULL
airbnb$neighbourhood_group <- as.factor(airbnb$neighbourhood_group)
airbnb$neighbourhood <- as.factor(airbnb$neighbourhood)
airbnb$room_type <- as.factor(airbnb$room_type)

airbnb$reviews_per_month<- ifelse(is.na(airbnb$reviews_per_month), mean(airbnb$reviews_per_month, na.rm = TRUE), airbnb$reviews_per_month)
last_half_year_rw <- ifelse(as.Date(airbnb$last_review)>as.Date("2019-01-07"),1,0)
last_half_year_rw <- ifelse(is.na(last_half_year_rw),0,last_half_year_rw)
airbnb$last_review <- last_half_year_rw
airbnb$last_review <- as.factor(airbnb$last_review)

#Pick data
manhattan <- airbnb[airbnb$neighbourhood_group=="Manhattan",]
table(manhattan$neighbourhood)
manhattan_five <- names(table(manhattan$neighbourhood)[order(-table(manhattan$neighbourhood))][1:5])
manhattan_five
airbnb <- subset(airbnb,airbnb$neighbourhood==manhattan_five)
airbnb$neighbourhood_group <- droplevels(airbnb$neighbourhood_group)
airbnb$neighbourhood <- droplevels(airbnb$neighbourhood)

str(airbnb)
View(airbnb)

#GGplot for price
library(ggplot2)
ggplot(airbnb, aes(price)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() + 
  ggtitle("Price Distribution") + 
            geom_vline(xintercept = mean(airbnb$price))+ scale_x_log10()
#Mean value
mean(airbnb$price)

# Split Data into Training and Testing in R 
train =airbnb[1:1695,]
test =airbnb[1696:2119,]

# Linear Regression Model
linear_model <- lm(price ~ neighbourhood + room_type  + number_of_reviews 
                   + last_review + reviews_per_month + availability_365, 
                   data=train)
summary(linear_model)
linear_pred <- predict(linear_model,test)
g=mean((linear_pred - test$price)^2)
print(c("linear-pred",g))

# SVM Model
library(kernlab)
SVM_pred <- ksvm(price ~ neighbourhood + room_type + number_of_reviews 
                 + last_review + reviews_per_month + availability_365, 
                 data=train, kernel = "vanilladot")
SVM_Price_Pred <- predict(SVM_pred, test)
g=mean((SVM_Price_Pred - test$price)^2)
print(c("SVM-pred",g))

# SVR Model (SVM for numeric variables)
library(e1071)
svr <- svm(price ~ neighbourhood + room_type + number_of_reviews 
           + last_review + reviews_per_month + availability_365, 
           data=train)
svr_pred <- predict(svr,test)
g=mean((svr_pred - test$price)^2)

