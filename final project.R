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
cor(linear_pred,test$price)

# SVM Model
library(kernlab)
SVM_pred <- ksvm(price ~ neighbourhood + room_type + number_of_reviews 
                 + last_review + reviews_per_month + calculated_host_listings_count + availability_365, 
                 data=train, kernel = "besseldot")
SVM_Price_Pred <- predict(SVM_pred, test)
g=mean((SVM_Price_Pred - test$price)^2)
print(c("SVM-pred",g))

# SVR Model (SVM for numeric variables)
library(e1071)
svr <- svm(price ~ neighbourhood + room_type + number_of_reviews 
           + last_review + reviews_per_month + calculated_host_listings_count + availability_365, 
           data=train)
svr_pred <- predict(svr,test)
g=mean((svr_pred - test$price)^2)
print(c("SVR-pred",g))


# Neural Network
## Create dummy variables for neighbourhood and room_type
#install.packages("dummies")
library(dummies)

airbnb$last_review <- unclass(airbnb$last_review)
airbnb_dummies <- dummy.data.frame(airbnb, sep = "_")
# remove space in column names
names(airbnb_dummies) <- gsub(" ", "_", names(airbnb_dummies))
names(airbnb_dummies) <- gsub("'", "", names(airbnb_dummies))
names(airbnb_dummies) <- gsub("/", "", names(airbnb_dummies))
summary(airbnb_dummies)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
airbnb_dummies_norm <- as.data.frame(lapply(airbnb_dummies, normalize))

train_nn =airbnb_dummies_norm[1:1695,]
test_nn =airbnb_dummies_norm[1696:2119,]



# Neural Network Model
# train the neuralnet model
library(neuralnet)

# a more complex neural network topology 2,1 hidden neurons

nn_model3 <- neuralnet(formula = price ~., data=train_nn, hidden=c(6), linear.output=FALSE, threshold=0.01)


## Step 4: Evaluating model performance ----
# obtain model results
nn_model_results3 <- compute(nn_model3, subset(test_nn, select = -c(price)))

# obtain predicted strength values
nn_predicted_price3 <- nn_model_results3$net.result
# examine the correlation between predicted and actual values
cor3 = cor(nn_predicted_price3, test_nn$price)[1,1]
RMSE.NN3 = (sum((test_nn$price - nn_predicted_price3)^2) / nrow(test_nn)) ^ 0.5

convert_price <- function(x) { 
  return (x * (max(airbnb_dummies$price) - min(airbnb_dummies$price))+min(airbnb_dummies$price))
}

## RMSE result
Best_adjusted_RMSE = (sum((convert_price(test_nn$price) - convert_price(nn_predicted_price3))^2) / nrow(test_nn)) ^ 0.5

#install.packages('NeuralNetTools')
library(ggplot2)
nn_feature_imp <- garson(nn_model3) + coord_flip()

## Feature Importance chart
nn_feature_imp
