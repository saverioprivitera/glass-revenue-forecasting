#####BUSINESS ECONOMICAL AND FINANCIAL DATA

library(readxl)
library(MASS)
library(ggplot2)
library(patchwork)
library(fpp2)
library(fpp3)
library(forecast) 
library(fable)
library(prophet)
library(dplyr) # Useful for data manipulation
library(stringr)
library(lubridate)
library(splines)
library(gam)
library(writexl)
library(lmtest)
library(car)


df = data.frame(read_excel("final_data.xlsx"))
head(df)
df = df[1:36,1:8]

View(df)

######Data transformations





######Data Analysis

p_23 <- ggplot(data = df[1:12,], aes(y = revenue)) + 
  geom_boxplot(fill = "darkgreen", color = "grey3") +
  labs(title = "Boxplot of the revenue in 2023", x = "2023") +
  scale_x_continuous(breaks = NaN) +
  theme_minimal()

p_24 <- ggplot(data = df[13:24,], aes(y = revenue)) + 
  geom_boxplot(fill = "indianred2", color = "grey3") +
  labs(title = "Revenue's boxplot in 2024", x = "2024") +
  scale_x_continuous(breaks = NaN) +
  theme_minimal()

p_25 <- ggplot(data = df[25:36,], aes(y = revenue)) + 
  geom_boxplot(fill = "blue2", color = "grey3") +
  labs(title = "Boxplot of the revenue in 2025", x = "2025") +
  scale_x_continuous(breaks = NaN) +
  theme_minimal()
p_23
p_24
p_25
p_23 + p_24 + p_25 & ylim(0, 76000)

#seasonal plot




######italian IPI over time 
plot(x = df$Month, y = df$Italy, type = "o")
ggplot(data = df, aes(x = Month, y = Italy)) +
  geom_point(size = 2.5, color = "coral") +
  geom_line(size = 1, color = "steelblue1") +
  labs(x = "Year", y = "IPI", title= "IPI in Italy") +
  theme_minimal()

######spanish IPI over time
ggplot(data = df, aes(x = Month, y = Spain)) +
  geom_point(size = 2.5, color = "tomato") +
  geom_line(size = 1, color = "olivedrab3") +
  labs(x = "Year", y = "IPI", title= "IPI in Italy") +
  theme_minimal()



boxplot(df[, c(2,4,6)], col = c('#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3', '#a6d854'))




revenue<-df$revenue
revenue<-na.omit(revenue)
revenues<-ts(revenue)
plot(revenue, type = 'o')


energy<-df$electr_gas
energy<-na.omit(energy)
Energy<-ts(energy)
plot(Energy, type = 'o')

spain<-df$Spain
spain<-na.omit(spain)
Spain<-ts(spain)
plot(spain, type = 'o')


acf(revenue)
acf(spain)
acf(energy)




arima_fit <- auto.arima(Spain)
summary(arima_fit)
previsione_spa <- forecast(arima_fit, h = 3)
plot(previsione_spa)
lines(fitted(arima_fit), col=2)
previsione_spa
coeftest(arima_fit)




holt_fit<-holt(Energy, h=3)
summary(holt_fit)
previsione_ener <- forecast(holt_fit)
plot(previsione_ener)
lines(fitted(holt_fit), col=2)



Arima_fit2<-auto.arima(Energy)
summary(Arima_fit2)
previsione_ener<-forecast(Arima_fit2, h = 3)
plot(previsione_ener)
previsione_ener         



newdata <- data.frame(
  Spain= c(78.78074 , 77.63629 , 77.63629 ),
  Energy = c(139.2663  , 140.7611  , 142.2560),
  tt = c(36, 37,38)
  )



#first linear model
fit<- tslm(revenues~Energy)
summary(fit)
AIC(fit)
checkresiduals(fit)
previsione <- forecast(fit, newdata = newdata)
plot(previsione)
lines(fitted(fit), col=2)
previsione



#second linear model
fit<- tslm(revenues~Energy + trend)
summary(fit)
AIC(fit)
checkresiduals(fit)
previsione <- forecast(fit, newdata = newdata)
plot(previsione)
lines(fitted(fit), col=2)
previsione



#third linear model
fit<- tslm(revenues~Energy + Spain)
summary(fit)
AIC(fit)
checkresiduals(fit)
previsione <- forecast(fit, newdata = newdata)
plot(previsione)
lines(fitted(fit), col=2)
previsione



#fourth linear model 
fit<- tslm(revenues~ Spain)
summary(fit)
AIC(fit)
checkresiduals(fit)
previsione <- forecast(fit, newdata = newdata)
plot(previsione)
lines(fitted(fit), col=2)
previsione



#model the orders
fit0 <- auto.arima(df$revenue)
checkresiduals(fit0)
# Ljung-Box p-value is larger than 0.05, so we can't reject 
# the null hypothesis (data are white noise)


mean(df$revenue, na.rm= TRUE)


Pacf(df$revenue)
Acf(df$revenue)
#perfect white noise






###

rev<-df$revenue
rev<-na.omit(rev)

tt <- c(1:35)

ener<-df$electr_gas
ener<-na.omit(ener)


sp <- df$Spain
sp <- na.omit(sp)


newdata_gam <- data.frame(  ener = c(139.2663, 140.7611, 142.2560),
                            sp = c(78.78074,77.63629,77.63629),
                            tt = c(36,37,38)) 


#----GAM model: all combinations ---------

g1 <- gam(revenues ~ s(tt)+s(ener)+s(sp))
g2 <- gam(revenues ~ s(tt)+s(ener))
g3 <- gam(revenues ~ s(tt)+s(sp))

g4 <- gam(revenues ~ lo(tt)+lo(ener)+lo(sp))
g5 <- gam(revenues ~ lo(tt)+lo(sp))
g6 <- gam(revenues ~ lo(tt)+lo(ener))

#- SUMMARY (F statistic) ------------

summary(g1)
summary(g2)
summary(g3) 

summary(g4) 
summary(g5)
summary(g6)

#- AIC -------------------------------

AIC(g1) 
AIC(g2)
AIC(g3) #Time + Spain (splines) best AIC: 799.09 

AIC(g4)
AIC(g5) 
AIC(g6)

#---------- GAM 3:  time + spain (splines)--------

g3 <- gam(revenues ~ s(tt)+s(sp))
summary(g3)
plot(g3, se=T)
AIC(g3)

plot(revenues,type="l",
     main="GAM: Time + Spain Production (splines)",
     xlab="Time",ylab="Revenue"
)
lines(fitted(g3),col="red")

res<- residuals(g3)
plot(res)
acf(res)
dwtest(g3)

previsionegam3 <- predict(g3, newdata = newdata_gam, se.fit = F)
previsionegam3

plot(rev,type="b",xlim=c(1,length(rev)+2),pch=18,
     main="GAM: Time + Spain production (splines) - Forecast",
     xlab="Time",ylab="Revenue", xaxt="n",yaxt="n")
axis(2, at = c(0,20000,40000,60000), las=1,labels = c(0,20000,40000,60000), cex.axis = 0.8)
axis(1, at = c(tt,37,38,39), labels = c(tt,37,38,39), cex.axis = 0.8)

lines(fitted(g3),col="red",lwd=2)
lines(c(35,newdata$tt),c(fitted(g3)[35],previsionegam3),col="blue",lwd=2)
points(newdata$tt,previsionegam3,col="blue",pch=19)








##############################PROPHET

#prophet
df_prophet  <- df[1:35,] %>% 
  select(Month, revenue) %>%
  rename(ds = Month, y = revenue)
df_prophet



df_prophet$cap <- 90000
head(df_prophet)

prophet_model <- prophet(df_prophet[1:35,], yearly.seasonality = T, weekly.seasonality = FALSE, 
                         daily.seasonality = FALSE, 
                         growth = "logistic")
# could add holiday effect since in august the revenue decreases  
# significantly with respect to July and September

#new dataframe for predictions
future_s <- make_future_dataframe(prophet_model, periods = 3, freq = "month", include_history = T)
future_s$cap <- 90000
future_s

forecast <- predict(prophet_model, future_s)
forecast

plot(prophet_model, forecast) + 
  add_changepoints_to_plot(prophet_model) +
  labs(x = "Time", y ="Revenue", title = "Prophet with a logistic trend")




### Now we add a holiday effect for august, since for all years there is a low revenue 
holidays_df <- data.frame(
  holiday = 'August holidays',
  ds = as.Date(c("2023-08-01", "2024-08-01", "2025-08-01", "2026-08-01")),
  lower_window = 0,  # No effect on previous months
  upper_window = 0   # No effect on following months
)
holidays_df

prophet_model_lh <- prophet(df_prophet[1:35,],
                            yearly.seasonality = T, weekly.seasonality = FALSE, 
                            daily.seasonality = FALSE, 
                            growth = "linear")

future_s_lh <- make_future_dataframe(prophet_model_lh, periods = 3, freq = "month", include_history = T)
future_s_lh$cap <- 90000
future_s_lh

forecast_sh <- predict(prophet_model_lh, future_s_lh)
forecast_sh


plot(prophet_model_lh, forecast_sh) + 
  add_changepoints_to_plot(prophet_model_lh) +
  labs(x = "Time", y ="Revenue", title = "Prophet with a linear trend")








##########SPLINES
df$t_num <- c(1:36) #add a variable index that numerate the obs



#----------- AIC Regression splines------------------------ 

aic_val <- numeric(length(1:10))

for(x in 1:10){
  mod_revenue <- lm(revenue ~ ns(t_num, df = x), data = df)
  aic_val[x] <- AIC(mod_revenue) 
}

aic_val #best: df = 1


#- MODEL: regression spline with df=1 ----------------------

mod_revenue <- lm(revenue ~ ns(t_num, df=1), data = df)
dwtest(mod_revenue)

summary(mod_revenue)
AIC(mod_revenue)


future <- seq(min(df$t_num), max(df$t_num), length.out = 200)
pred_revenue <- predict(mod_revenue, newdata = data.frame(t_num = future))

pred_revenue_df <- data.frame(
  t_num = future,
  revenue_pred = pred_revenue)

plot(df$t_num, df$revenue, type = "b",xaxt = "n", yaxt = "n",
     xlab = "Time", ylab = "Revenue",
     main = "Regression Spline - Revenue")

lines(future, pred_revenue, col = "red", lwd = 2)
AIC(mod_revenue)
axis(1, at = df$t_num, labels = df$t_num, cex.axis = 0.8)

#------- 3 months forecast ------------------

N <- nrow(df)
t_future <- seq(N + 1, N + 3, by = 1) # Creiamo t_num per i prossimi 6 mesi

forecast_revenue <- predict(mod_revenue, newdata = data.frame(t_num = t_future))

forecast_df <- data.frame(
  t_num = t_future,
  revenue_forecast = forecast_revenue
)


plot(df$t_num, df$revenue,
     xlab = "Time", ylab = "Revenue",
     type="b", xaxt = "n", yaxt = "n",
     main = "Regression spline - Revenue + Forecast",
     xlim = c(min(df$t_num), max(forecast_df$t_num))
)

tempo_new <- seq(min(df$t_num), max(df$t_num), length.out = 200)

lines(tempo_new, pred_revenue, col = "red", lwd = 2)              # curva storica liscia
lines(forecast_df$t_num, forecast_df$revenue_forecast, col = "red", lwd = 2, lty = 2)  # forecast

axis(2, at = c(0,20000,40000,60000), las=1,labels = c(0,20000,40000,60000), cex.axis = 0.8)
axis(1, at = c(df$t_num,37,38,39), labels = c(df$t_num,37,38,39), cex.axis = 0.8)

AIC(mod_revenue)

#--------Model: SMOOTHING SPLINE -------------------------------

mod_smooth <- smooth.spline(x = df$t_num[!is.na(df$revenue)],
                            y = df$revenue[!is.na(df$revenue)],
                            cv = TRUE)
summary(mod_smooth)



pred_smooth <- predict(mod_smooth, x = tempo_new)

pred_smooth_df <- data.frame(
  t_num = pred_smooth$x,
  revenue_pred = pred_smooth$y
)

#-------------- forecast for 3 months ----------------
N <- nrow(df)
t_future <- seq(N + 1, N + 3, by = 1)

forecast_smooth <- predict(mod_smooth, x = t_future)

forecast_smooth_df <- data.frame(
  t_num = forecast_smooth$x,
  revenue_forecast = forecast_smooth$y
)

pred_smooth
forecast_smooth_df

# smoothing spline plot
plot(df$t_num, df$revenue,
     pch = 19, col = "gray40",
     xlim = c(0,40), type="b",
     xlab = "Time", ylab = "Revenue",
     xaxt="n",yaxt="n",
     main = "Smoothing spline - Revenue")


lines(pred_smooth$x, pred_smooth$y, col = "red", lwd = 2)
points(forecast_smooth_df$t_num, forecast_smooth_df$revenue_forecast, col = "blue", pch = 19)
lines(forecast_smooth_df$t_num, forecast_smooth_df$revenue_forecast, col = "blue", lwd = 2, lty = 2)

axis(2, at = c(0,20000,40000,60000), las=1,labels = c(0,20000,40000,60000), cex.axis = 0.8)
axis(1, at = c(df$t_num,37,38,39), labels = c(df$t_num,37,38,39), cex.axis = 0.8)

legend("topleft",
       legend = c("Observed data", "Smoothing Spline", "3 months forecast"),
       col = c("gray40", "blue", "red"),
       lwd = c(NA, 2, 2),
       pch = c(19, NA, 19),
       bty = "n")

mod_smooth$spar
mod_smooth$lambda
mod_smooth$df
summary(mod_smooth)








###################EXPONENTIAL SMOOTHING

# 3. Create Time Series Object
# Frequency = 12 implies monthly data. Start year is 2023.
ts_revenue <- ts(df$revenue, start = c(2023, 1), frequency = 12)
ts_revenue <- na.omit(ts_revenue)

# --- MODEL 1: Simple Exponential Smoothing (SES) ---
# Used for data with no trend and no seasonality.
# 'alpha' is the smoothing parameter.
ses_model <- ses(ts_revenue, h = 3) 
summary(ses_model)
ses_model$mean

# --- MODEL 2: Holt's Linear Trend Method ---
# Used for data with trend but no seasonality.
holt_model <- holt(ts_revenue, h = 3)
summary(holt_model)


# --- MODEL 3: Holt-Winters Seasonal Method ---
# Used for data with both trend and seasonality.
# We use 'additive' seasonality based on the visual inspection.
hw_model <- HoltWinters(ts_revenue, seasonal = "additive")
hw_forecast <- forecast(hw_model, h = 3)

# 4. Plotting the comparison
# Set up a 2x2 plotting grid
par(mfrow=c(1,1))

# Plot 1: Original Data
plot(ts_revenue, main="Original Revenue Data", ylab="Revenue", col="black", lwd=2, 
     xlab = "Time", xaxt = "n")
axis(side = 1, 
     at = c(2023.0, 2023.5, 2024.0, 2024.5, 2025.0, 2025.5), 
     labels = c("Jan 2023", "Jul 2023", "Jan 2024", "Jul 2024", "Jan 2025", "Jul 2025"))
legend("topleft", 
       legend = c("Observed", "Fitted (History)", "Forecast"), 
       col = c("black", "red", "blue"), 
       lty = c(1, 2, 2), 
       cex = 0.8)

# Plot 2: SES
plot(ses_model, main="Simple Exponential Smoothing", ylab="Revenue", flty=2, type = "o",
     xlab = "Time", xaxt = "n")
axis(side = 1, 
     at = c(2023.0, 2023.5, 2024.0, 2024.5, 2025.0, 2025.5), 
     labels = c("Jan 2023", "Jul 2023", "Jan 2024", "Jul 2024", "Jan 2025", "Jul 2025"))

lines(ses_model$fitted, col = "red", lty = 2)
legend("topleft", 
       legend = c("Observed", "Fitted (History)", "Forecast"), 
       col = c("black", "red", "blue"), 
       lty = c(1, 2, 2), 
       cex = 0.8)

# Plot 3: Holt's
plot(holt_model, ylab="Revenue", flty=2, type = "o",
     xlab = "Time", xaxt = "n", main = "Holt Exponential Smoothing")
axis(side = 1, 
     at = c(2023.0, 2023.5, 2024.0, 2024.5, 2025.0, 2025.5), 
     labels = c("Jan 2023", "Jul 2023", "Jan 2024", "Jul 2024", "Jan 2025", "Jul 2025"))
lines(holt_model$fitted, col = "red", lty = 2)
legend("topleft", 
       legend = c("Observed", "Fitted (History)", "Forecast"), 
       col = c("black", "red", "blue"), 
       lty = c(1, 1, 1),
       cex = 0.8)

# Plot 4: Holt-Winters
plot(hw_forecast, main="Holt-Winters Exponential Smoothing", ylab="Revenue", flty=2,
     xlab = "Time", xaxt = "n")
axis(side = 1, 
     at = c(2023.0, 2023.5, 2024.0, 2024.5, 2025.0, 2025.5), 
     labels = c("Jan 2023", "Jul 2023", "Jan 2024", "Jul 2024", "Jan 2025", "Jul 2025"))
lines(hw_model$fitted[,1], col="red", lty=2) # Add fitted line


par(mfrow=c(1,1))

#Accuracy Metrics (RMSE, MAE, etc.) to see which is best model
print(accuracy(ses_model))

print(accuracy(holt_model))

print(accuracy(hw_forecast))







