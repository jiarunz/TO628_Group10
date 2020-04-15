#read data
airbnb <- read.csv("/Users/helenlee/Desktop/TO/AB_NYC_2019.csv")
str(airbnb)
View(airbnb)

#Clean data
airbnb$id <- NULL
airbnb$name <- NULL
airbnb$host_id <- NULL
airbnb$host_name <- NULL
airbnb$latitude <- NULL
airbnb$longitude <- NULL
str(airbnb)
airbnb$reviews_per_month<- ifelse(is.na(airbnb$reviews_per_month), mean(airbnb$reviews_per_month, na.rm = TRUE), airbnb$reviews_per_month)
airbnb$neighbourhood_group <- as.factor(airbnb$neighbourhood_group)
airbnb$neighbourhood <- as.factor(airbnb$neighbourhood)
airbnb$room_type <- as.factor(airbnb$room_type)

#Pick data
manhattan <- airbnb[airbnb$neighbourhood_group=="Manhattan",]
table(manhattan$neighbourhood)
manhattan_five <- names(table(manhattan$neighbourhood)[order(-table(manhattan$neighbourhood))][1:5])
airbnb <- subset(airbnb,airbnb$neighbourhood==manhattan_five)

last_half_year_rw <- ifelse(as.Date(airbnb$last_review)>as.Date("2019-01-07"),1,0)
last_half_year_rw <- ifelse(is.na(last_half_year_rw),0,last_half_year_rw)
airbnb$last_review <- last_half_year_rw
airbnb$last_review <- as.factor(airbnb$last_review)
airbnb$neighbourhood <- as.factor(airbnb$neighbourhood)
airbnb$room_type <- as.factor(airbnb$room_type)
summary(last_half_year_rw)
str(last_half_year_rw)

str(airbnb)
View(airbnb)

# Split Data into Training and Testing in R 
sample_size = floor(0.8*nrow(airbnb))
set.seed(666)

# randomly split data in r
picked = sample(seq_len(nrow(airbnb)),size = sample_size)
train =airbnb[picked,]
test =airbnb[-picked,]