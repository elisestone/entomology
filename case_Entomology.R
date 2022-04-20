temp <- read_xlsx('Data Scientist Task.xlsx', sheet ="temp") #temperature data
features <- read_xlsx('Data Scientist Task.xlsx', sheet ="features") #feature data
coord<- read_xlsx('Data Scientist Task.xlsx', sheet ="coord") #coordinates

#DATA WRANGLING
#steps: calculate mean temperature, humidity, mean time spent in the field for each device
#check if is needed to modify lat and long variables
#join tables

library("tidyverse")
library("lubridate")
library("dplyr")

temp$Time <- strptime(temp$Time, format = "%Y-%m-%d %H:%M:%S") #transforming date and time as posixt

temp_diff <- temp %>% arrange(device_ID, Time) %>%
              group_by(device_ID) %>%
              mutate(diff_time = as.numeric(Time - lag(Time))) %>%
              replace(is.na(.), 0) #calculating time differences between measurements


temp_group <- group_by(temp_diff, device_ID) #grouping the temp data by device

temp_group_descrip <- temp_group %>% summarise(
mean_temperature = mean(temperature),
mean_humidity = mean(humidity),
sum_time = sum(diff_time)) #calculating mean temperature and humidity and sum of time spent in the field by device

dataset_join<- full_join(features, coord, by= "device_ID")
dataset_final<- full_join(dataset_join, temp_group_descrip, by= "device_ID") #joining tables to form the final dataset  

View(dataset_final)


##################DATA ANALYSIS

#start by investigating if there is a spatial pattern related to device quality 
# but first convert coordinates to decimal format

library("ape")
library("sp")

dataset_final$Lat <- dataset_final$Latitude %>%
  sub('°', ' ', .) %>%
  sub("'", ' ', .) %>%
  sub('"', ' ', .) %>%
  sub('S', '', .)
dataset_final$Long <- dataset_final$Longitude %>%
  sub('°', ' ', .) %>%
  sub("'", ' ', .) %>%
  sub('"', ' ', .) %>%
  sub('W', '', .)

dataset_final$Lat <- conv_unit(dataset_final$Lat, from = "deg_min_sec", to = "dec_deg")
dataset_final$Long <- conv_unit(dataset_final$Long, from = "deg_min_sec", to = "dec_deg")

dataset_final$Lat <- as.numeric(dataset_final$Lat)*(-1) 
dataset_final$Long <- as.numeric(dataset_final$Long)*(-1) 


# Calculating Moran's I measure of spatial autocorrelation

dist<- as.matrix(dist(cbind(dataset_final$Long, dataset_final$Lat)))
dist_inv<- 1/dist #generating a matrix of inverse distance weights
diag(dist_inv)<- 0

Moran.I(dataset_final$quality_device, dist_inv) #no evidence of spatial autocorrelation

####### contribution of explanatory variables to device quality 

#transform the group variable as a factor of 3 levels
#normalize quantitative variables

dataset_final$group <- as.factor(dataset_final$group)
dataset_norm <- copy(dataset_final)

# quantitative columns to scale
cols <- c("n_insects", 'cost_device', 'mean_temperature', 'mean_humidity', 'sum_time')

# normalization function
range01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# applying the function
dataset_norm[cols] <- lapply(dataset_norm[cols], range01)

# visualizing variables relationships

dataset_final %>%
  gather(-device_ID, -quality_device, -Latitude, -Longitude, -Lat, -Long, -group,
         key = "var", value = "value") %>%
  ggplot(aes(x = value, y = quality_device, color = group)) +
  geom_point()+
  facet_wrap(~ var, scales = 'free')+
  theme_bw()
 
# visualizing the correlations between variables
library("corrplot")
cols2 <- c("n_insects", 'quality_device', 'cost_device', 'mean_temperature', 
           'mean_humidity', 'sum_time')
dataset_corr<- cor(dataset_final[cols2], method = 'pearson') #correlation matrix

corrplot(dataset_corr, 
         method = "color",
         sig.level = 0.05,
         order = "original",
         diag = FALSE,
         addCoef.col = "black",
         tl.col = "black",
         type = "upper",
         insig = "blank",
         tl.srt = 75) #correlogram

#### GLM relating explanatory variables with device quality

library('stats')
poisson_model1<- glm(quality_device ~ n_insects + group + mean_temperature + mean_humidity + sum_time, 
                     family = poisson(link = 'log'),
                     data = dataset_norm)
summary(poisson_model1)

poisson_model2<- glm(quality_device ~ n_insects + cost_device + mean_temperature + mean_humidity + sum_time, 
                     family = poisson(link = 'log'),
                     data = dataset_norm)
summary(poisson_model2)

poisson_model2.1<- glm(quality_device ~ n_insects + cost_device + mean_temperature + sum_time, 
                     family = poisson(link = 'log'),
                     data = dataset_norm)
summary(poisson_model2.1)

poisson_model2.2<- glm(quality_device ~ n_insects + mean_temperature +sum_time, 
                       family = poisson(link = 'log'),
                       data = dataset_norm)
summary(poisson_model2.2) 

poisson_model2.3<- glm(quality_device ~ n_insects + mean_temperature, 
                       family = poisson(link = 'log'),
                       data = dataset_norm)
summary(poisson_model2.3) 

par(mfrow = c(2,2))
plot(poisson_model2.3) #checking the residuals



#sugestions: survival analysis, wind speed