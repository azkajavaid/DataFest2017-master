
library(readr)
library(dplyr)
require(mosaic)
library(ROCR)

dest <- read.csv("destFile.csv")
clicks <- read.csv("clickFile.csv")

comp_data <- left_join(clicks, dest)
comp_data2 <- comp_data[c(1:31)]
omit <- na.omit(comp_data2)
omit <- subset(omit, is_booking == 1)
slicks <- sample_n(omit, 100000) #analyze only 100,000 observations 

slicks$user_location_longitude <- as.numeric(as.character(slicks$user_location_longitude))
slicks$user_location_latitude <- as.numeric(as.character(slicks$user_location_latitude))

small <- slicks

small$orig_destination_distance <- as.numeric(as.character(small$orig_destination_distance))
small$is_mobile <- as.factor(small$is_mobile)
small$is_package <- as.factor(small$is_package)
small$channel<- as.factor(small$channel)
small$stayDur <- as.Date(as.character(small$srch_co), format="%Y-%m-%d")-
  as.Date(as.character(small$srch_ci), format="%Y-%m-%d")
small$stayDur <- as.numeric(small$stayDur)
small$is_booking <- as.factor(small$is_booking)

#create predictor for foreign/domestic distinction
small$user_location_country <- as.character(small$user_location_country)
small$hotel_country <- as.character(small$hotel_country)
small <- small %>% mutate(domestic = ifelse(user_location_country == hotel_country, 1, 0))
small$prop_is_branded <- as.factor(small$prop_is_branded)
small$prop_starrating <- as.factor(small$prop_starrating)
small$distance_band <- as.factor(small$distance_band)
small$hist_price_band <- as.factor(small$hist_price_band)
small$popularity_band <- as.factor(small$popularity_band)
small$cnt <- as.numeric(small$cnt)
small$user_location_country <- as.factor(small$user_location_country)
small$hotel_country <- as.factor(small$hotel_country)
small <- small %>% mutate(USstatus = ifelse(user_location_country == "UNITED STATES OF AMERICA", 1, 0))
small$USstatus <- as.factor(small$USstatus)
small$srch_children_cnt <- as.numeric(small$srch_children_cnt)
small <- small %>% mutate(NoChild = ifelse(srch_children_cnt > 0, 0, 1))
small$NoChild <- as.factor(small$NoChild)
small$date_time <- as.POSIXct(small$date_time)
small$month <- as.factor(months(small$date_time)) #months returns the actual months

is_family <- ifelse(small$srch_children_cnt > 0, TRUE, FALSE)
is_solo <- ifelse(small$srch_children_cnt == 0 & small$srch_adults_cnt == 1, TRUE, FALSE)
is_group <- ifelse(small$srch_children_cnt == 0 & small$srch_adults_cnt > 2, TRUE, FALSE)
is_couple <- ifelse(small$srch_children_cnt == 0 & small$srch_adults_cnt == 2, TRUE, FALSE)

#create class groupings predictor 
small$class <- as.factor(ifelse(is_family == TRUE, "family", ifelse(is_solo == TRUE, "solo", 
                                ifelse(is_group == TRUE, "group", ifelse(is_couple == TRUE,
                                                                         "couple", NA)))))

small <- subset(small, is_booking == 1)
small2 <- small
small <- small2

small3 <- small %>% group_by(user_location_country, hotel_country, class, domestic) %>% tally(.)

#get latitude and longitude for the originating and destination locations and merge with small3 dataset 
subsmall <- small %>% select(user_location_country, user_location_latitude, 
                 user_location_longitude, hotel_country, srch_destination_latitude, srch_destination_longitude)

small4 <- small3 %>% inner_join(subsmall, by = c("user_location_country" = "user_location_country", 
                                       "hotel_country" = "hotel_country"))
#select one longitude and latitude as representative of that country 
user <- unique(small4$user_location_country)
hotel <- unique(small4$hotel_country)

#records the unique latitude and longitude for each country 
uniqGeo <- data.frame()
for (i in unique(small4$hotel_country))
{
  one <- subset(small4, hotel_country == i)
  oneOb <- head(one, 1) #take one instance of that country 
  lat <- oneOb$srch_destination_latitude
  lon <- oneOb$srch_destination_longitude
  geo <- cbind(lat, lon, i)
  geo1 <- as.data.frame(geo)
  uniqGeo <- rbind(geo1, uniqGeo)
}
save(uniqGeo, file = "uniqLocGeo.Rda")
smallgeo <- small4
smallgeo <- smallgeo %>% inner_join(uniqGeo, by = c("user_location_country" = "i")) #inner join by user_location_country
smallgeo <- plyr::rename(smallgeo, replace = c("lon" = "userLong"))
smallgeo <- plyr::rename(smallgeo, replace = c("lat" = "userLat"))
smallgeo <- smallgeo %>% inner_join(uniqGeo, by = c("hotel_country" = "i"))

smaller <- smallgeo %>% select(user_location_country, hotel_country, class, domestic, n, userLat, userLong, lat, lon)
smaller <- unique(smaller)
smaller$num <- c(1:1283)
smaller <- plyr::rename(smaller, replace = c("userLat" = "user_location_latitude"))
smaller <- plyr::rename(smaller, replace = c("userLong" = "user_location_longitude"))
smaller <- plyr::rename(smaller, replace = c("lat" = "srch_destination_latitude"))
smaller <- plyr::rename(smaller, replace = c("lon" = "srch_destination_longitude"))

smaller$user_location_latitude <- as.numeric(as.character(smaller$user_location_latitude))
smaller$user_location_longitude <- as.numeric(as.character(smaller$user_location_longitude))

smaller$srch_destination_latitude <- as.numeric(as.character(smaller$srch_destination_latitude))
smaller$srch_destination_longitude <- as.numeric(as.character(smaller$srch_destination_longitude))

slicksUser <- smaller %>% select(user_location_latitude, user_location_longitude, domestic, class, n, num)
slicksDest <- smaller %>% select(srch_destination_latitude, srch_destination_longitude, domestic, class, n, num)

slicksUser <- plyr::rename(slicksUser, replace = c("user_location_latitude" =
                                                     "srch_destination_latitude"))
slicksUser <- plyr::rename(slicksUser, replace = c("user_location_longitude" =
                                                     "srch_destination_longitude"))
common <- rbind(slicksUser, slicksDest)

common2 <- subset(common, num > 700)

common <- common2
soloDat <- subset(common, class == "solo")
family <- subset(common, class == "family")
group <- subset(common, class == "group")
couple <- subset(common, class == "couple")

solo100 <- sample(soloDat, size = 100)
family100 <- sample(family, size = 100)
group100 <- sample(group, size = 100)
couple100 <- sample(couple, size = 100)

library(leaflet)
library(magrittr)

######################################Building Map to show distribution of flights by class (solo, family, group, couple)
#solo flights foreign (just looking at foreign flights: 100)
map1 <- leaflet() %>% addTiles() %>% 
  addCircles(data = solo100, lng = ~srch_destination_longitude, 
             lat= ~srch_destination_latitude, #group='circles', 
             weight = ~sqrt(n),
             color='black')

for (i in unique(solo100$num))
{
  sub <- subset(solo100, num == i & domestic == 0) #just looking at foreign flights 
  map1 <- map1 %>% addPolylines(data = sub, lng = ~srch_destination_longitude, 
                                lat = ~srch_destination_latitude, group = ~num, 
                                weight = 2, color = '#330000') 
}
map1


#family flights (100 foreign observations)
map2 <- leaflet() %>% addTiles() %>% 
  addCircles(data = family100, lng = ~srch_destination_longitude, 
             lat= ~srch_destination_latitude, #group='circles', 
             weight = ~sqrt(n),
             color='black')

for (i in unique(family100$num))
{
  sub <- subset(family100, num == i & domestic == 0) #just looking at foreign flights 
  map2 <- map2 %>% addPolylines(data = sub, lng = ~srch_destination_longitude, 
                                lat = ~srch_destination_latitude, group = ~num, 
                                weight = 2, color = '#6600ff') 
}
map2

#group flights (100 foreign observations)
map3 <- leaflet() %>% addTiles() %>% 
  addCircles(data = group100, lng = ~srch_destination_longitude, 
             lat= ~srch_destination_latitude, #group='circles', 
             weight = ~sqrt(n),
             color='black')

for (i in unique(group100$num))
{
  sub <- subset(group100, num == i & domestic == 0) #just looking at foreign flights 
  map3 <- map3 %>% addPolylines(data = sub, lng = ~srch_destination_longitude, 
                                lat = ~srch_destination_latitude, group = ~num, 
                                weight = 2, color = '#336600') 
}
map3

#couple flights (100 foreign observations)
map4 <- leaflet() %>% addTiles() %>% 
  addCircles(data = couple100, lng = ~srch_destination_longitude, 
             lat= ~srch_destination_latitude, #group='circles', 
             weight = ~sqrt(n),
             color='black')

for (i in unique(couple100$num))
{
  sub <- subset(couple100, num == i & domestic == 0) #just looking at foreign flights 
  map4 <- map4 %>% addPolylines(data = sub, lng = ~srch_destination_longitude, 
                                lat = ~srch_destination_latitude, group = ~num, 
                                weight = 2, color = "#993300") 
}
map4

#################################Predictive Analysis and Machine Learning models 
#Logistic regression
logmod <- glm(is_booking ~ USstatus + orig_destination_distance + is_mobile + is_package + channel + stayDur +
                srch_adults_cnt + srch_children_cnt + domestic + prop_is_branded + prop_starrating + distance_band + 
                hist_price_band + popularity_band + cnt, data = train, family=binomial(link='logit'))

varImp(logmod) #find variable importance 

#Decision tree 
library(rpart)
form <- as.formula("is_booking ~ USstatus + orig_destination_distance + is_mobile + is_package + channel + stayDur +
srch_adults_cnt + srch_children_cnt + domestic + prop_is_branded + prop_starrating + distance_band + 
                   hist_price_band + popularity_band + cnt")
mod_tree <- rpart(form, data = train)
train <- train %>%
  mutate(income_dtree = predict(mod_tree, type = "class"))
confusion <- tally(income_dtree ~ is_booking, data = train, format = "count")
confusion 
sum(diag(confusion))/nrow(train)

#Random Forest
library(randomForest)
trainall <- train %>% select(is_booking, USstatus, orig_destination_distance, is_mobile, is_package, channel, stayDur, srch_adults_cnt,
                 srch_children_cnt, domestic, prop_is_branded, prop_starrating, distance_band, hist_price_band,
                 popularity_band, cnt)
trainall <- na.omit(trainall)
trainall$USstatus<- as.factor(trainall$USstatus)
mod_forest <- randomForest(form, data = trainall, ntree = 100, mtry = 3)
mod_forest
sum(diag(mod_forest$confusion))/nrow(train)
dat <- varImp(mod_forest) 

varImpPlot(mod_forest)

##Variable importance
library(tibble)
importance(mod_forest) %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  arrange(desc(MeanDecreaseGini))


##############Machine learning to model user class 
dat4 <- read.csv("dataset7.csv") #500000 observations  for hotel type 
dat33 <- read.csv("comp_data3.csv")
dat4 <- na.omit(dat4)
dat33 <- na.omit(dat33)

set.seed(364)
n <- nrow(dat4)
test_idx <- sample.int(n, size = round(0.2*n))
train <- dat4[-test_idx,]
test <- dat4[test_idx, ]

set.seed(364)
n <- nrow(dat33)
test_idx <- sample.int(n, size = round(0.2*n))
train <- dat33[-test_idx,]
test <- dat33[test_idx, ]

#use_mobile : at any point, they used a mobile device 
dat4$use_mobile <- as.factor(dat4$use_mobile)
dat4$use_package <- as.factor(dat4$use_package)
dat4$channel <- as.factor(dat4$channel)
dat4$is_domestic <- as.factor(dat4$is_domestic)
form <- as.formula("hotel_type ~ use_mobile + use_package + channel + 
                   stay_duration + class + times + totaltimes + advance + is_domestic")

dat33$use_mobile <- as.factor(dat33$use_mobile)
dat33$use_package <- as.factor(dat33$use_package)
dat33$channel <- as.factor(dat33$channel)

form <- as.formula("class ~ use_mobile + use_package + channel + stay_duration + times +
                   totaltimes + advance + ave_star + range_star + 
                   ave_price + ave_dist + range_price + range_dist + num_branded + num_bookings")

#tree: model 
mod_tree <- rpart(form, data = train)
train <- train %>%
  mutate(income_dtree = predict(mod_tree, type = "class"))
confusion <- tally(income_dtree ~ class, data = train, format = "count")
confusion 
sum(diag(confusion))/nrow(train)

#Random forest 
library(randomForest)
mod_forest <- randomForest(form, data = train, ntree = 100, mtry = 3)
mod_forest

pred <- predict(mod_forest, test, type = "class") #test on the test set 
conf <- table(test$hotel_type, pred) 
sum(diag(conf))/sum(conf) #accuracy (59% for class) #accuracy (52% for hotel?)
varImpPlot(mod_forest, scale = FALSE)
tab1 <- varImp(mod_forest)
save(tab1, file = "ClassClusterImp.Rda")
plot(imp)

######Improved variable importance plot 
lh <- load("HotelClusterImp.Rda")
library(data.table)
setDT(tab, keep.rownames = TRUE)[]

tab <- tab %>%
  arrange(desc(Overall))
tab$rn <- as.factor(tab$rn)
levels(tab$rn)[7] <- "total_times"

ggplot(tab, aes(x = reorder(rn, Overall), y = Overall, fill = Overall)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("Variable") + ylab("Variable Importance") + ggtitle("Hotel Type Importance Plot (Accuracy = 52%)")

#########Variable importance for clusters
lh <- load("ClassClusterImp.Rda")
library(data.table)
setDT(tab1, keep.rownames = TRUE)[]

tab1 <- tab1 %>%
  arrange(desc(Overall))

tab1$rn <- as.factor(tab1$rn)
levels(tab1$rn)[7] <- "total_times"

ggplot(tab1, aes(x = reorder(rn, Overall), y = Overall, fill = Overall)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("Variable") + ylab("Variable Importance") + ggtitle("Hotel Type Importance Plot (Accuracy = 62%)")

##########Class logistic regression
logmod <- glm(hotel_type ~ use_mobile + use_package + channel + 
                stay_duration + class + times + totaltimes + advance + is_domestic,
              data = train, family=binomial(link='logit'))
varImp(logmod) #find variable importance 


logmod2 <- glm(class ~ use_mobile + use_package + channel + stay_duration + times +
                 totaltimes + advance + ave_star + range_star + 
                 ave_price + ave_dist + range_price + range_dist + num_branded + num_bookings,
               data = train, family=binomial(link='logit'))
summary(logmod2)

#predicting class levels against other predictors 
classmodLog <- multinom(class ~ use_mobile + use_package + channel + stay_duration + times +
                          totaltimes + advance + ave_star + range_star + 
                          ave_price + ave_dist + range_price + range_dist + num_branded + num_bookings, data = train)

classmodLogHotel <- multinom(hotel_type ~ use_mobile + use_package + channel + 
                               stay_duration + class + times + totaltimes + advance + is_domestic,
                             data = train)
