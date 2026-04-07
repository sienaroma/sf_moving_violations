#import packages
library(readr)
library(ggplot2)
library(zoo)
library(Metrics)
library(forecast)
library(astsa)
library(dplyr)

#confirm no outliers
data <- read_csv("monthly_moving_violation_2011.csv")
Q1 <- quantile(data$Stop_Count,0.25)
Q3 <- quantile(data$Stop_Count,0.75)
IQR <- IQR(data$Stop_Count)
lower_bound <- Q1 - (1.5*IQR)
upper_bound <- Q3 + (1.5*IQR)
outliers <- subset(data, data$Stop_Count < lower_bound | data$Stop_Count > upper_bound)
print(outliers)

#check time series plot
plot(data$Stop_Count, type="b", main="Time Series Plot of Traffic Stops",ylab="Count of Stops",xlab="Month")

#log transform data to reduce variance
data_log <- data %>%
  mutate(Stop_Count = log(Stop_Count))

#create training and testing set (test set 8/2015-6/2016)
data_log$Month <- as.yearmon(data_log$Month)
test_set_start_date <- as.yearmon("Aug 2015")
train_set <- subset(data_log,Month <=test_set_start_date)
test_set <- subset(data_log,Month>test_set_start_date)

#plot training and testing on time series plot
ggplot() +
  geom_line(data = train_set, aes(x = Month, y = Stop_Count, color = "Training"), size = 1)+
  geom_line(data = test_set, aes(x = Month, y = Stop_Count, color = "Testing"), size = 1)+
  labs(
    title= "SF Traffic Stops - Training and Testing Sets",
    x = "Month",
    y = "Stops") +
  scale_color_manual(values = c("Training" = "#12355B", "Testing" = "#D72638"),name = "Traffic Stops")+
  theme_minimal() +
  theme(plot.title = element_text(size=20))

#set data as a time series
series_ts <- ts(train_set$Stop_Count,frequency=12)

#Create triple exponential smoothing models
tes_seasonal_add <- HoltWinters(series_ts, seasonal = "additive")
tes_seasonal_mul <- HoltWinters(series_ts, seasonal = "multiplicative")
tes_seasonal_add_forecast <- forecast(tes_seasonal_add, h = nrow(test_set))
tes_seasonal_mul_forecast <- forecast(tes_seasonal_mul, h = nrow(test_set))
exsm_tes_forecast_df <- data.frame(
  Month = test_set$Month,
  TESAdd = tes_seasonal_add_forecast$mean,
  TESMul = tes_seasonal_mul_forecast$mean)
#plot results on time series
ggplot()+
  geom_line(data = train_set, aes(x = Month, y=Stop_Count, color="Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y=Stop_Count, color="Testing"), size = 1) +
  geom_line(data = exsm_tes_forecast_df, aes(x=Month, y=TESAdd, color="TESAdd"), size = 1)+
  geom_line(data = exsm_tes_forecast_df, aes(x=Month, y=TESMul, color="TESMul"), size = 1)+
  labs(
    title = "SF Traffic Stops - Tripple Exponential Smoothing Predictions",
    x = "Month",
    y = "Stops")+
  scale_color_manual(values = c("Training"="#12355B","Testing"="#D72638","TESAdd"="orange","TESMul"="purple"),name="Traffic Stops")+
  theme_minimal()+
  theme(plot.title=element_text(size=20))

#pull acf and pacf plots
acf2(series_ts)

#create ARIMA models
arima_model1 <- Arima(series_ts,order=c(2,0,1),seasonal=list(order=c(2,0,0),period=12))
arima_model2 <- Arima(series_ts,order=c(2,0,0),seasonal=list(order=c(2,0,0),period=12))

arima_forecast_df <- data.frame(
  Month = test_set$Month,
  ARIMA1 = arima_forecast1$mean,
  ARIMA2 = arima_forecast2$mean)
#plot results on time series
ggplot()+
  geom_line(data = train_set, aes(x = Month, y=Stop_Count, color="Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y=Stop_Count, color="Testing"), size = 1) +
  geom_line(data=arima_forecast_df, aes(x=Month,y=ARIMA1, color="ARIMA1"),size=1)+
  geom_line(data=arima_forecast_df, aes(x=Month,y=ARIMA2, color="ARIMA2"),size=1)+
  labs(
    title = "SF Traffic Stops - ARIMA Predictions",
    x = "Month",
    y = "Stops")+
  scale_color_manual(values = c("Training"="#12355B","Testing"="#D72638","ARIMA1"="orange","ARIMA2"="purple"),name="Model")+
  theme_minimal()+
  theme(plot.title=element_text(size=20))

#test ARIMA vs triple exponential smoothing models
#compare RMSE, MAPE, and MAE
all_model_data <- data.frame(
  Month = test_set$Month,
  Stop_Count = test_set$Stop_Count,
  TESAdd = tes_seasonal_add_forecast$mean,
  TESMul = tes_seasonal_mul_forecast$mean,
  ARIMA1 = arima_forecast1$mean,
  ARIMA2 = arima_forecast2$mean)

mae_values <- c()
mape_values <- c()
rmse_values <- c()

for (col in names(all_model_data)[3:ncol(all_model_data)]) {
  mae_values <- c(mae_values, mae(all_model_data$Stop_Count, all_model_data[[col]]))
  mape_values <- c(mape_values, mape(all_model_data$Stop_Count, all_model_data[[col]]))
  rmse_values <- c(rmse_values, rmse(all_model_data$Stop_Count, all_model_data[[col]]))}
model_test_set_metrics <- data.frame(
  Model = names(all_model_data)[3:ncol(all_model_data)],
  MAE = mae_values,
  MAPE = mape_values,
  RMSE = rmse_values)

model_test_set_metrics

#forecast using ARIMA 1 model and 90% confidence
full_series = ts(data_log$Stop_Count,frequency=12)
forecast <- sarima.for(full_series,n.ahead=12,p=2,d=0,q=1,P=2,D=0,Q=0,S=12)
lower_bound <- exp(forecast$pred - 1.645*forecast$se)
upper_bound <- exp(forecast$pred + 1.645*forecast$se)

#check assumptions
#confirmed no significant residuals
#confirmed normality
#confirmed no trend
sarima(series_ts,2,0,1,2,0,0,12)