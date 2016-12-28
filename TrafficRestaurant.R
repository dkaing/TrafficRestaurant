library(httr)
library(httpuv)
library(jsonlite)
library(base64enc)
library(rgdal)
library(sp)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rgeos)
library(ggthemes)
library(ggmap)
library(grid)
library(gridExtra)
library(chron)
library(geosphere)
library(rpart)
library(caret)

## New York Traffic Data
files <- list.files()[grep("green_tripdata", list.files())]
df <- lapply(files, read.csv)
df <- do.call(rbind, df)
df <- df[, c(2,3,6,7,8,9,11)]
colnames(df) <- c("pdt", "ddt", "plon", "plat", "dlon", "dlat","trip_dis")
df$p_hr <- format(as.POSIXct(df$pdt), "%H")
df$p_t <- times(format(as.POSIXct(df$pdt), "%H:%M:%S"))*24
df$d_t <- times(format(as.POSIXct(df$ddt), "%H:%M:%S"))*24
df$t_dif <- as.numeric(df$d_t-df$p_t)
df <- df[df$t_dif >0,]
df$day <- weekdays(as.Date(df$pdt))

## map by community district

cd <- readOGR(path.expand("~/Downloads/nycd_16c/nycd.shp"), 
              layer = "nycd")

cd <- spTransform(cd, CRS("+proj=longlat +datum=WGS84"))


ny_community_dist <- function(lon, lat) {
      coords <- data.frame(lon = lon, lat = lat)
      points <- SpatialPoints(coords)
      proj4string(points) <- proj4string(cd)
      cd <- over(points, cd)$BoroCD
      return(cd)
}

df$pcd <- ny_community_dist(df$plon, df$plat)

df$dcd <- ny_community_dist(df$dlon, df$dlat)

df <- na.omit(df)
df <- df[df$t_dif<2,]


## predictive modeling shows that location, hour, and day can predict
## average trip time
mod_df <- df[, c("plon", "plat", "dlon", "dlat", 
                 "trip_dis", "p_hr", 
                 "t_dif", "pcd", "dcd", "day")]

trainingPartition <- createDataPartition(mod_df$t_dif, p = 0.7, list = F)
training <- mod_df[trainingPartition,]
testing <- mod_df[-trainingPartition,]

lmod <- lm(t_dif~., data = training)

lprediction <- predict(lmod, testing)
lm_rmse <- sqrt(mean((testing$t_dif-lprediction)^2))
lm_rmse
### lm_rmse = 0.09936403 hr or 5.961842 minutes

fit <- rpart(t_dif ~., 
             method = "anova", data = training)
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

dt_pred <- predict(pfit, testing)
dt_rmse <- sqrt(mean((testing$t_dif-dt_pred)^2))
dt_rmse
### dt_rmse = 0.1035984 hr or 6.215904

## Yelp API access key

consumerKey = "JrI5XBhB9nSSmoXAIRc0mw"
consumerSecret = "Sm0sFnWqvcHSK-bhdunmNJs63As"
token = "qcv-LAtUjrIRIAXcgJUHzp_GkJoGvcOr"
token_secret = "3NtNNnkjXVyoljc7wzp23sNkx6I"

myapp <- oauth_app("YELP",
                   key = consumerKey,
                   secret = consumerSecret)
sig <-sign_oauth1.0(myapp,token=token, 
                    token_secret=token_secret)

## get all restaurants data with relevant attributes 

get_rest <- function(term, nycity) {
      seqby20 <- seq(0,200,20)
      df <- NULL
      for(i in 1:length(seqby20)) {
            args <- list(term=term,
                         location=paste(nycity, ", New York", sep = ""),
                         limit=20, offset = seqby20[i])
            result <- GET("https://api.yelp.com/v2/search/", 
                          sig, query=args)
            if(length(result$content)<1000) break
            d1 <- fromJSON(toJSON(content(result)))
            d1 <- data.frame(d1)
            df1 <- data.frame(rating = as.numeric(as.character(
                  d1$businesses.rating)), 
                              review_count = as.numeric(as.character(
                                    d1$businesses.review_count)), 
                              name = as.character(
                                    d1$businesses.name),
                              url = as.character(
                                    d1$businesses.url), 
                              phone = as.character(
                                    d1$businesses.phone), 
                              address = as.character(
                                    d1$businesses.location$address),
                              city = as.character(
                                    d1$businesses.location$city), 
                              lat = as.numeric(as.character(
                                    d1$businesses.location$coordinate$latitude)), 
                              lon = as.numeric(as.character(
                                    d1$businesses.location$coordinate$longitude)))
            
            df <- rbind(df,df1)
      }
      df <- data.frame(df)
      return(df)
}

## extract data by minimum and maximum rating

tsl <- function(term, nycity, min_rating, max_rating) {
      df <- get_rest(term, nycity)
      min_rating <- as.numeric(min_rating)
      max_rating <- as.numeric(max_rating)
      df <- df[df$rating <=max_rating & df$rating >= min_rating, ]
      return(df)
}

## map query to nyc community district and inclue maximum distance arg
## (maximum dist from origin) to function


tslmd <- function(term, nycity, min_rating, max_rating, orig_lat, 
                  orig_lon, max_dist) {
      query_df <- tsl(term, nycity, 4,5)
      query_df$cd <- ny_community_dist(query_df$lon, query_df$lat)
      orig_cd <- ny_community_dist(orig_lon, orig_lat)
      
      orig_coord <- data.frame(lon = rep(orig_lon, nrow(query_df)),
                               lat = rep(orig_lat, nrow(query_df)))
      query_coord <- query_df[, c("lon","lat")]
      
      query_df$dist <- (distHaversine(orig_coord, 
                                      query_coord)*0.621371)/1000
      query_df <- query_df[query_df$dist < max_dist,]
      query_df <- na.omit(query_df)
      return(query_df)
}

## get the top 10 restaurants using performance score

top10 <- function(term, nycity, min_rating, max_rating, orig_lat, 
                  orig_lon, max_dist) {
      query_df <- tslmd(term, nycity, min_rating,max_rating ,
                        orig_lat, orig_lon,max_dist)
      query_df$score <- (query_df$rating/max(query_df$rating))*
            (query_df$review_count/max(query_df$review_count))
      query_df <- query_df[order(query_df$score, decreasing = T),]
      return(query_df[1:10,])
}

##  output traffic information - average trip time

traffic_df <- function(term, nycity, min_rating, max_rating, 
                       orig_lat, orig_lon, max_dist) {
      query_df <- top10(term, nycity, min_rating, max_rating, 
                        orig_lat, orig_lon, max_dist)
      
      orig_cd <- ny_community_dist(orig_lon, orig_lat)
      orig_pickup_df <- df[df$pcd == orig_cd,]
         
      result <- NULL
      for(i in 1:nrow(query_df)) {
            if(sum(orig_pickup_df$dcd == query_df$cd[i])>0) {
            origin_df <- orig_pickup_df[orig_pickup_df$dcd== 
                                              query_df$cd[i],]
            summary_df <- origin_df %>% group_by(p_hr) %>% 
                  summarise(mean(t_dif))
            summary_df$name <- query_df$name[i]
            summary_df <- summary_df %>% inner_join(query_df, "name")
            }
            result <- rbind(result, summary_df)
      }
      return(result)
            
}

## filter traffic data by hour

traffic_hr_df <- function(term, nycity, min_rating, max_rating, 
                          orig_lat, orig_lon, max_dist, hr) {
      results <- traffic_df(term, nycity, min_rating, max_rating, 
                            orig_lat, orig_lon, max_dist)
      results$p_hr <- as.numeric(results$p_hr)
      colnames(results)[2] <- "mean_time"
      hr_df <- results[results$p_hr==hr, ]
      return(hr_df)
}
