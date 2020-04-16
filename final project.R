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
price_plot <- ggplot(data=airbnb, aes(x=price)) + geom_histogram(fill = "red")
price_plot

# Split Data into Training and Testing in R 
train =airbnb[1:1695,]
test =airbnb[1696:2119,]
