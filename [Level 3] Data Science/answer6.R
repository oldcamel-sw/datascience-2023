################################################################################
##### ProDS Professional 공개문제                                          #####
#####                                                                      #####
################################################################################
### Load packages
if (!require("data.table")) install.packages("data.table")
if (!require("imputeTS")) install.packages("imputeTS")
if (!require("reshape")) install.packages("reshape")
if (!require("glmnet")) install.packages("glmnet")

library(data.table)
library(imputeTS)
library(reshape)
library(glmnet)

### Load data
weather = fread('weather.csv')
weather_valid = fread('weather_valid.csv')
temperature = fread('temperature.csv')
data = fread('data.csv')
building_metadata = fread('building_metadata.csv')

str(weather); dim(weather)
str(weather_valid); dim(weather_valid)
str(temperature); dim(temperature)
str(data); dim(data)
str(building_metadata); dim(building_metadata)

################################################################################
### Preprocess
weather_dataset = weather[order(region_id, datetime)]

# Check non-null unique values by region_id
weather_cnt = weather_dataset[, lapply(.SD, function(x) length(unique(x))-max(is.na(x))), region_id]
weather_cnt

weather_cnt2 = data.table(melt(weather_cnt, id.vars=c("region_id")))
weather_cnt2 = weather_cnt2[variable != 'datetime']

# check only NA groups
weather_nulls = weather_cnt2[value==0]
weather_nulls

# missing value imputation - except NA groups
for(i in 1:nrow(weather_cnt2)) {
  if(weather_cnt2[i, value] != 0) {
    r = weather_cnt2[i, region_id]
    c = weather_cnt2[i, variable]
    print(paste0("region: ",r,", variable: ",c))
    eval(parse(text=paste0("weather_dataset[region_id==",r,", ",c,":= na.interpolation(",c,", option='linear')]")))
  }
}
print(weather_dataset)

# clouds
weather_dataset[, cloud := ifelse(cloud>=8, 8, (cloud%/%2)*2)]
table(weather_dataset$cloud)

weather_dataset

################################################################################
### Question 1
weather_9 = weather_dataset[region_id==9, .(datetime, temperature)]
colnames(weather_9)[2] = "temperature_interpolation"
setkeyv(weather_9, c("datetime"))
setkeyv(weather_valid, c("datetime"))
weather_join = weather_9[weather_valid]
rmse1 = weather_join[, sqrt(mean((temperature - temperature_interpolation)^2))]

# Answer 1
ans1 = round(rmse1, 3)
ans1

################################################################################
### Question 2

# Step 2-1
dt3 = weather_dataset[region_id %in% c(0,1), .(datetime, region_id, temperature)]
dt3 = as.data.table(cast(dt3, datetime ~ region_id, mean))
colnames(dt3)[-1] = paste("region_id_", colnames(dt3)[-1], sep="")
dt3

# Step 2-2
setkeyv(dt3, c("datetime"))
setkeyv(temperature, c("datetime"))
dt3 = dt3[temperature]
dim(dt3)
dt3

# Step 2-3 - use melt for easy calculation
dt3_melt = as.data.table(melt(dt3, id.vars=c("datetime","region_id_0","region_id_1")))
dt3_cities = dt3_melt[, .(rmse_0=sqrt(mean((region_id_0 - value)^2)), 
                          rmse_1=sqrt(mean((region_id_1 - value)^2))), variable]
dt3_cities

# Asnwer 2
region_0 = dt3_cities[order(rmse_0)][1, variable]
region_1 = dt3_cities[order(rmse_1)][1, variable]
ans2 = paste0(region_0, ", ", region_1)
ans2

################################################################################
### Question 3

# Check
data[, .N, .(datetime, user_id)][N>1]

# Step 3-1
data_train = data[!is.na(generate)]
data_train
dim(data_train) # 198897 3

setkeyv(data_train, c("user_id"))
setkeyv(building_metadata, c("user_id"))
data_train = building_metadata[data_train]

# Step 3-2
setkeyv(data_train, c("datetime", "region_id"))
setkeyv(weather_dataset, c("datetime", "region_id"))

dt3 = weather_dataset[data_train]
dt3

# Step 3-3
dt3[, datetime:= as.POSIXct(datetime)]
dt3[, hour:= as.numeric(format(as.POSIXct(datetime), format = "%H"))]
dt3

cols = c("region_id","hour","generate","temperature","cloud",
         "dew_point","rain_hourly","air_pressure","wind_direction")
data_agg = dt3[, ..cols][, lapply(.SD, mean), .(region_id, hour)]

# Step 3-4
data_agg = na.omit(data_agg)

# Step 3-5, Step 3-6
cor_mat = matrix(0, nrow=7, ncol=7)
for(r in sort(unique(data_agg$region_id))) {
  cor_reg = cor(data_agg[region_id==r,-c("region_id","hour")])
  print(r)
  print(cor_reg)
  cor_mat = cor_mat + cor_reg
}
cor_mat = cor_mat/length(unique(data_agg$region_id))
cor_mat

# Answer 3
ans3 = sort(round(abs(cor_mat[1,2:7]), 3), decreasing=T)[1]
ans3

################################################################################
### Question 4

# Step 4-1
weather_dataset[, datetime := as.POSIXct(datetime)]
weather_dataset[, hour:= as.numeric(format(datetime, format = "%H"))]
avg_temp = weather_dataset[, .(avg_temp = mean(temperature)), .(region_id, hour)]

# Step 4-2
avg_temp[, max_temp := max(avg_temp), region_id]
max_hour = avg_temp[avg_temp==max_temp][order(region_id),.(region_id, hour, max_temp)]
max_hour[, shift := 14-hour]
max_hour = max_hour[, .(region_id, shift)]

setkeyv(max_hour, c("region_id"))
setkeyv(weather_dataset, c("region_id"))

weather_dataset = weather_dataset[max_hour]
weather_dataset[, datetime := datetime + shift*60*60]

# Step 4-3
setkeyv(weather_dataset, c("datetime", "region_id"))
data_train[, datetime:= as.POSIXct(datetime)]
setkeyv(data_train, c("datetime", "region_id"))

dt4 = data_train[weather_dataset]

# Step 4-4
dt4[, generate_log := log(generate+1)]

# Step 4-5
dt4[, month := as.numeric(format(datetime, '%m'))]
dt4[, day := as.numeric(format(datetime, '%d'))]
dt4[, hour := as.numeric(format(datetime, '%H'))]

# Step 4-6
dt4 = dt4[, .(generate_log, area, construct_year, num_floors, temperature, 
              cloud, dew_point, rain_hourly, air_pressure, wind_direction,
              hour, day, month, usage)]
dt4 = dt4[!is.na(construct_year)]
dt4 = dt4[, num_floors := ifelse(is.na(num_floors), 0, num_floors)]
dt4 = dt4[, cloud := ifelse(is.na(cloud), 0, cloud)]
dt4 = dt4[, rain_hourly := ifelse(is.na(rain_hourly), 0, rain_hourly)]
dt4 = dt4[, air_pressure := ifelse(is.na(air_pressure), 0, air_pressure)]

master_dataset = dt4

# Step 4-7
lr_mdl = lm(generate_log~., master_dataset[,-c("usage")])
lr_pred = predict(lr_mdl, master_dataset)
rmse4 = sqrt(mean((master_dataset$generate_log - lr_pred)^2))

# Answer 4
ans4 = round(rmse4, 3)
ans4

################################################################################
### Question 5

# Step 5-1
master_dataset[, usage_numeric := mean(generate_log), usage]

# Step 5-2
master_dataset[, area_log := log(area+1)]
master_dataset[, num_floors_log := log(num_floors+1)]

# Step 5-3
dt5 = master_dataset[, .(generate_log, area_log, construct_year, num_floors_log, 
                         temperature, cloud, dew_point, rain_hourly, air_pressure, 
                         wind_direction, hour, day, month, usage_numeric)]

# Step 5-4
y = dt5[, generate_log]
x = as.matrix(dt5[, -c("generate_log")])

ridge_mdl = glmnet(x, y, alpha=0, lambda=0.001, maxit=1000, thresh=1e-3)
ridge_pred = predict(ridge_mdl, x)
rmse5 = sqrt(mean((master_dataset$generate_log - ridge_pred)^2))

# Answer 5
ans5 = round(rmse5, 3)
ans5

################################################################################