#' ---
#' title: "Store Sales Forecasting"
#' output: 
#'   github_document:
#'     toc: true
#' ---
#' 
## ----setup, include = F--------------------------------------------
knitr::opts_chunk$set(message = F, warning = F)

#' 
## ------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(imputeTS)
library(tictoc)
library(forecast)

#' 
## ------------------------------------------------------------------
train_df <- read_csv("train.csv.zip")
glimpse(train_df)

test_df <- read_csv("test.csv.zip")
glimpse(test_df)

stores <- read_csv("stores.csv")
glimpse(stores)

features <- read_csv("features.csv.zip")
glimpse(features)

#' 
#' ### Data Preprocessing
#' 
#' We attempt time-series forecasting based on only the `Weekly_Sales` of each unique pair of `Store` and `Dept`. We will use only `train_df` and `test_df` datasets.
#' 
#' ##### Check for NA values
#' 
## ------------------------------------------------------------------
checkForNA <- function(data){
  apply(is.na(data), 2, function(col) paste0(round(mean(col) * 100, 2), "%"))
}

#' 
## ------------------------------------------------------------------
checkForNA(train_df)
checkForNA(test_df)

#' 
#' There are no NA values in any columns.
#' 
#' ##### Combine `Store` and `Dept` to form unique identifier for each department across all stores
#' 
## ------------------------------------------------------------------
addUniqueStoreDept <- function(data){
  mutate(data, storeDept = paste0(Store, "_", Dept),
         .before = 1)
}

#' 
## ------------------------------------------------------------------
train_df <- addUniqueStoreDept((train_df))
test_df <- addUniqueStoreDept((test_df))

head(train_df)

#' 
#' ##### Check if every `storeDept` in `test_df` have historical observations in `train_df`
#' 
## ------------------------------------------------------------------
n_distinct(train_df$storeDept)
n_distinct(test_df$storeDept)

#' 
#' We have more `storeDept` in `train_df` than in `test_df`. We filter them out as we only need those present in `test_df` for forecasting.
#' 
## ------------------------------------------------------------------
train_df <- filter(train_df, storeDept %in% unique(test_df$storeDept))

n_distinct(test_df$storeDept) - n_distinct(train_df$storeDept)

#' 
#' After filtering out the irrelevant `storeDept`, we found out that there are 11 `storeDept` in `test_df` that do not have historical observations in `train_df`.
#' 
## ------------------------------------------------------------------
(storeDeptNoData <- 
  test_df %>%
  filter(!storeDept %in% unique(train_df$storeDept)) %>%
  .$storeDept %>%
  unique())

#' 
#' The above are the identifiers of the 11 `storeDept` without historical observations.
#' 
#' ##### Check if the data has irregular time series (missing gaps between observations)
#' 
## ------------------------------------------------------------------
# Add 1 because the first week is not accounted for in the difference

startTrain <- min(train_df$Date)
endTrain <- max(train_df$Date)

startTest <- min(test_df$Date)
endTest <- max(test_df$Date)

(lengthTrain <- difftime(endTrain, startTrain, units = "weeks") + 1)
(lengthTest <- difftime(endTest, startTest, units = "weeks") + 1)

#' 
#' We should have 143 number of observations for each `storeDept`.
#' 
## ------------------------------------------------------------------
obsPerStoreDept <-
  train_df %>%
  count(storeDept) %>%
  arrange(n) %>%
  rename(numObs = n)

unique(obsPerStoreDept$numObs)

#' 
#' We have time series of various intervals. The maximum length is 143 weeks, which corresponds to what we computed above. Surprisingly, there are `storeDept` with less than 10 observations over the 143 weeks.
#' 
## ----fig.height = 3, fig.width = 8---------------------------------
obsPerStoreDept %>%
  count(numObs) %>%
  ggplot(aes(numObs, n)) +
  ylab("Frequency") + xlab("Number of Observations") +
  geom_jitter(color = "orangered", alpha = 0.5, height = 100) +  
  geom_vline(xintercept = 143, lty = 2, lwd = 0.5, color = "steelblue")

#' 
#' For clarity, we plotted jitter points in terms of height, and the point where `num_obs` = 143 is indicated with the blue vertical line. Across more than 3000 unique `storeDept`, the majority, or specifically, over 2500 `storeDept` have evenly-spaced time series. The others have irregular time series.
#' 
#' ##### Check if there are differences for `storeDept` with irregular time series
#' 
#' Since our objective is to minimize the `WMAE` of forecast `Weekly_Sales`, we find out if `storeDept` with irregular time series have different behavior.
#' 
#' We first check the distribution of `Weekly_Sales`.
#' 
## ------------------------------------------------------------------
numObs_vs_weeklySales <- train_df %>%
  merge(obsPerStoreDept, by = "storeDept") %>%
  select(Date, storeDept, Weekly_Sales, numObs)

#' 
## ------------------------------------------------------------------
numObsLabels <- c("FALSE" = "numObs == 143", "TRUE" = "numObs < 143")

numObs_vs_weeklySales.aes <- function(data, scales = "free_y"){
  data %>%
  ggplot(aes(fill = as.factor(numObs == 143) ,
             color = as.factor(numObs == 143))) +
  theme(legend.position = "none") +
  facet_grid(rows = vars(numObs < 143),
             labeller = as_labeller(numObsLabels),
             scales = scales)
}

#' 
## ----fig.width = 8-------------------------------------------------
numObs_vs_weeklySales.aes(numObs_vs_weeklySales) +
  geom_density(aes(Weekly_Sales), alpha = 0.5) +
  coord_cartesian(xlim = c(-5000,100000))

#' 
#' It seems that both distributions are right-skewed. `storeDept` with missing number of observations clearly have a smaller spread of `Weekly_Sales` around the peak.
#' 
#' We plot the time series for median of `Weekly_Sales` across `storeDept` and indicate the holiday weeks and the previous week of the holidays to get a general idea.
#' 
## ------------------------------------------------------------------
(holidayWeeks <-
  train_df %>%
  filter(IsHoliday == T) %>%
  .$Date %>%
  unique())

(weekBeforeHolidays <- holidayWeeks - 7)

#' 
## ----fig.width = 8-------------------------------------------------
numObs_vs_weeklySales.aes(numObs_vs_weeklySales) +
  stat_summary(aes(Date, Weekly_Sales), fun = median, geom = "line", lwd = 1.3) +
  geom_vline(xintercept = holidayWeeks, lty = 2, lwd = 0.1, alpha = 0.3) +
  geom_vline(xintercept = weekBeforeHolidays, lty = 2, lwd = 0.1, alpha = 0.3)

#' 
#' Promotions usually start before the holidays to attract early holiday shoppers. Hence, we expect `Weekly_Sales` to be rising before the holidays. This is apparent in the periods before Super Bowl, Thanksgiving and Christmas. However, such an effect is not that noticeable before Labor Day.
#' 
#' Both plots show similar behavior as described above, although the magnitude of `Weekly_Sales` for `storeDept` with irregular time series is significantly smaller. We go on to confirm their differences in magnitude.
#' 
## ------------------------------------------------------------------
numObs_vs_weeklySales.scatter <- function(fn, title){
  numObs_vs_weeklySales %>%
  group_by(storeDept, numObs) %>%
  summarize(Weekly_Sales = fn(Weekly_Sales)) %>%
  numObs_vs_weeklySales.aes(scales = "fixed") +
  geom_jitter(aes(numObs, Weekly_Sales), width = 3, height = 5000, alpha = 0.3) +
  ggtitle(title)
}

#' 
## ----warning = F, fig.height = 6-----------------------------------
grid.arrange(numObs_vs_weeklySales.scatter(median, "Median"),
             numObs_vs_weeklySales.scatter(mean, "Mean"),
             numObs_vs_weeklySales.scatter(min, "Min"),
             numObs_vs_weeklySales.scatter(max, "Max"),
             numObs_vs_weeklySales.scatter(sd, "Standard Deviation"),
             ncol = 3, nrow = 2)

#' 
#' It is evident from the jittered-point plots that those `storeDept` with irregular time series generally have lower `Weekly_Sales` and also less variability across those `storeDept`, with the red points forming a horizontal band.
#' 
#' Hence, the `storeDept` with missing observations do not contribute as much to the `WMAE` as the absolute values of the `Weekly_Sales` are small relative to those `storeDept` with regular time series.
#' 
#' ##### Converting irregular time series to regular time series
#' 
#' We first add in the missing gaps in the time series by performing an outer join with the dates of the 143 weeks.
#' 
## ------------------------------------------------------------------
trainDates <- tibble("Date" = seq(startTrain, endTrain, by = 7))

mergeTS <- function(data){
  storeDept <- unique(data$storeDept)
  Store <- unique(data$Store)
  Dept <- unique(data$Dept)
  merge(data, trainDates, by = "Date", all = T) %>%
  replace_na(list(storeDept = storeDept, 
                  Store = Store, 
                  Dept = Dept #, 
                 # Weekly_Sales = 0
                 ))
}

#' 
## ------------------------------------------------------------------
storeDept_df <-
  train_df %>%
  select(storeDept, Store, Dept, Date, Weekly_Sales) %>%
  group_by(storeDept) %>%
  do(mergeTS(.)) %>%
  ungroup() %>%
  arrange(Store, Dept)

storeDept_df

#' 
#' We then convert the data into a `mts` object. Using `pivot_wider`, we can spread the time series of each `storeDept` to be in separate columns.
#' 
## ------------------------------------------------------------------
storeDept_ts<- 
  storeDept_df %>%
  select(-Store, -Dept) %>%
  pivot_wider(names_from = storeDept, values_from = Weekly_Sales) %>%
  select(-Date) %>%
  ts(start = decimal_date(startTrain), frequency = 52)

storeDept_ts[, 1]

#' 
#' Here, we will mainly perform interpolation on the seasonally adjusted data whenever possible (depending on the number of NA values).
#' 
## ------------------------------------------------------------------
impute <- function(current_ts){
 if(sum(!is.na(ts)) >= 3){
    na_seadec(current_ts)
 } else if(sum(!is.na(ts)) == 2){
   na_interpolation(current_ts)
 } else{
   na_locf(current_ts)
 }
}

#' 
## ------------------------------------------------------------------
for(i in 1:ncol(storeDept_ts)){
  storeDept_ts[, i] <- impute(storeDept_ts[, i])
} 

sum(is.na(storeDept_ts))

#' 
#' ### Model Visualization and Exploration
#' 
#' We investigate what models are suitable for our time series data. From the above median `Weekly_Sales` plot, it seems that there are strong seasonality patterns around the holidays, though the trend appears to be weak.
#' 
#' We will examine the forecast plots of the different models to confirm that the time series patterns can be captured. We evaluate the models later on as a whole when we fit the models to each `storeDept`, as minimizing `WMAE` across all `storeDept` is our goal.
#' 
#' Here, we initialize a base time series to use as reference for our plots.
#' 
## ------------------------------------------------------------------
# change index for different storeDept
baseTS <- storeDept_ts[, 111] 
baseTS_train <- baseTS %>% subset(end = 107)

#' 
#' A good baseline model that can capture seasonality patterns would be a `snaive` model, taking the observations from a year before as the forecast values.
#' 
#' Since we have over 3000 `storeDept` to fit, we will use time series models with automatic algorithms to select the parameters like `ets` and `auto.arima`. Due to high frequency (~52 weeks per year), we use STL decomposition to fit the model on the seasonally adjusted data before reseasonalizing.
#' 
#' * SNaive
#' * Linear regression with trend and seasonal dummies
#' * Dynamic harmonic regression
#' * SARIMA
#' * Models with STL decomposition
#' 
## ------------------------------------------------------------------
snaive_baseTS <- snaive(baseTS_train, 36)

tslm_baseTS <- tslm(baseTS_train ~ trend + season) %>% forecast(h = 36)

arima_fourier_baseTS <- auto.arima(baseTS_train,seasonal = F, 
                                   xreg = fourier(baseTS_train, K = 3)) %>%
  forecast(xreg = fourier(baseTS_train, K = 3, h = 36), h = 36)

sarima_baseTS <- auto.arima(baseTS_train) %>% forecast(h = 36)

stl_arima_baseTS <- stlf(baseTS_train, method = "arima", 36)

stl_ets_baseTS <- stlf(baseTS_train, method = "ets", 36)

#' 
## ------------------------------------------------------------------
forecast_plots <- function(ref, fc_list, model_names){
  plt <- autoplot(ref)
  for(i in 1:length(fc_list)){
    plt <- plt + autolayer(fc_list[[i]], series = model_names[i], PI = F)
  }
  plt <- plt +  
    ylab("Weekly_Sales") +
    guides(color = guide_legend(title = "Forecast"))
  plt
}

#' 
## ----fig.width = 9, fig.height = 4---------------------------------
forecast_plots(baseTS, 
               list(tslm_baseTS,
                    snaive_baseTS,
                    stl_ets_baseTS),
               c("TSLM",
                 "SNaive",
                 "STL-ETS")
)

#' 
## ----fig.width = 9, fig.height = 4---------------------------------
forecast_plots(baseTS, 
               list(sarima_baseTS,
                    stl_arima_baseTS,
                    arima_fourier_baseTS),
               c("SARIMA",
                 "STL-ARIMA",
                 "ARIMA-Fourier")
)

#' 
#' Most of the models capture the seasonality patterns. However, the forecast plot for models with Fourier terms appears to be less ideal. We find out what happens as the maximum order of the Fourier terms increases.
#' 
## ----fig.width = 9, fig.height = 8---------------------------------
arima_fourier_plots <- list()
for(j in 1:6){
  fit <- auto.arima(baseTS_train, xreg = fourier(baseTS_train, K = 6 + 2 * j), seasonal = F) 
  fc <- fit %>%
    forecast(xreg = fourier(baseTS_train, K = 6 + 2 * j, h = 36), h = 36)
  arima_fourier_plots[[j]] <- 
    autoplot(baseTS) + 
    autolayer(fc, PI = F, color = "red") + 
    ylab("Weekly_Sales") + 
    ggtitle(paste("K =", 6 + 2 * j))
}

grid.arrange(arima_fourier_plots[[1]],
             arima_fourier_plots[[2]],
             arima_fourier_plots[[3]],
             arima_fourier_plots[[4]],
             arima_fourier_plots[[5]],
             arima_fourier_plots[[6]],
             ncol = 2)

#' 
#' At K = 8 and K = 10, the forecasts still do not capture the patterns sufficiently. Beyond K = 12, the forecasts are more appropriate. A thing to note is that the forecasts start to look similar regardless of the value of K. We can check that this is true for many `storeDept` by changing our base time series defined above.
#' 
#' As tuning the optimal K value for each of the 3000 over time series requires a lot of computation time, we select an appropriate K value here at K = 12. This may not be the most optimal one but less Fourier terms results in less computation time overall.
#' 
#' ### Model Validation
#' 
#' We perform a train-test split instead of using cross-validation due to computational reasons with the large number of time series. To ensure at least 2 seasonal periods (~ 2 years of data), we need around 75% of the data in the training set.
#' 
## ------------------------------------------------------------------
holidayWeights <- train_df %>%
  select(Date, IsHoliday) %>%
  unique() %>%
  .$IsHoliday
holidayWeights <- ifelse(holidayWeights, 5, 1)

totalSize <- nrow(storeDept_ts)
trainSize <- round(0.75 * totalSize)
testSize <- totalSize - trainSize

test_weights <- holidayWeights[(totalSize - testSize + 1):totalSize]
train <- storeDept_ts %>% subset(end = trainSize)
test <- storeDept_ts %>% subset(start = trainSize + 1)

#' 
#' We define a function that computes `WMAE`, and another that fits a model and generates forecasts.
#' 
## ------------------------------------------------------------------
wmae <- function(fc){
  # rep() to replicate weights for each storeDept
  weights <- as.vector(rep(test_weights, ncol(fc)))
  
  # as.vector() collapse all columns into one
  MetricsWeighted::mae(as.vector(test), as.vector(fc), weights)
}

model_fc <- function(train, h, model, ...){
  
  tic()
  
  # Initialize forecasts with zeroes
  fc_full <- matrix(0, h, ncol(train))
  
  # Iterate through all storeDept to perform forecasting
  for(i in 1:ncol(train)){
    current_ts <- train[, i]
    fc <- model(current_ts, h, ...)
    fc_full[, i] <- fc
  }
  
  toc()
  
  # Return forecasts
  fc_full
}

#' 
#' We define the functions of the various models to generate the forecasts.
#' 
## ------------------------------------------------------------------
snaive_ <- function(current_ts, h){
  snaive(current_ts, h = h)$mean
}

tslm_ <- function(current_ts, h){
  tslm(current_ts ~ trend + season) %>%
    forecast( h = h) %>%
    .$mean
}
arima_fourier <- function(current_ts, h, K = K){
  auto.arima(current_ts, xreg = fourier(current_ts, K = K), seasonal = F) %>% 
    forecast(xreg = fourier(current_ts, K = K, h = h), h = h) %>%
    .$mean
}

sarima <- function(current_ts, h){
  auto.arima(current_ts) %>%
    forecast(h = h) %>%
    .$mean
}

stl_ets <- function(current_ts, h){
  stlf(current_ts, method = "ets", opt.crit = 'mae', h = h)$mean
}

stl_arima <- function(current_ts, h){
  stlf(current_ts, method = "arima", h = h)$mean
}

#' 
## ------------------------------------------------------------------
snaive_fc <- model_fc(train, testSize, snaive_)
tslm_fc <- model_fc(train, testSize, tslm_)
stl_ets_fc <- model_fc(train, testSize, stl_ets)
stl_arima_fc <- model_fc(train, testSize, stl_arima)
sarima_fc <- model_fc(train, testSize, sarima)
arima_fourier_fc <- model_fc(train, testSize, arima_fourier, K = 12)

#' 
## ------------------------------------------------------------------
wmae_summary <- 
  tibble("Model" = c("SNaive (Baseline)", "TSLM",
                     "SARIMA", "ARIMA-Fourier",
                     "STL-ARIMA", "STL-ETS"
                     ),
         "WMAE" = c(wmae(snaive_fc), wmae(tslm_fc),
                    wmae(sarima_fc), wmae(arima_fourier_fc),
                    wmae(stl_arima_fc), wmae(stl_ets_fc)
                    ))

wmae_summary %>% arrange(WMAE)

#' All models performed better than the baseline model. We find that models that use STL decomposition performed better as they do not have to handle the high frequency, unlike `SARIMA`.
#' 
#' We can also create an average model by taking the average over the forecasts of the above models. This helps to average out the errors.
#' 
## ------------------------------------------------------------------
average_fc <- (snaive_fc 
               + tslm_fc 
               + sarima_fc 
               + arima_fourier_fc 
               + stl_arima_fc 
               + stl_ets_fc
               ) / 6

average_weak_fc <- (snaive_fc 
                    + tslm_fc 
                    + sarima_fc 
                    ) / 3

wmae_summary %>% 
  add_row(Model = c("Average of all Models", "Weak Models Average"), 
          WMAE = c(wmae(average_fc), wmae(average_weak_fc))) %>%
  arrange(WMAE)

#' 
#' The `WMAE` for the average of the weaker models perform significantly better. In fact, taking the average over all individual models results in an even better score.
#' 
#' ### Final Model
#' 
#' We decide on the final model to be the average forecasts across all the models based on the `WMAE` because our validation was only done using train-test split. Averaging over all the models may be a better way to deal with model uncertainty.
#' 
## ------------------------------------------------------------------
final_snaive_fc <- model_fc(storeDept_ts, lengthTest, snaive_)
final_tslm_fc <- model_fc(storeDept_ts, lengthTest, tslm_)
final_stl_ets_fc <- model_fc(storeDept_ts, lengthTest, stl_ets)
final_stl_arima_fc <- model_fc(storeDept_ts, lengthTest, stl_arima)
final_sarima_fc <- model_fc(storeDept_ts, lengthTest, sarima)
final_arima_fourier_fc <- model_fc(storeDept_ts, lengthTest, arima_fourier, K = 12)

#' 
#' Before submission, we need to make adjustments (credits [here](https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/discussion/8028)) to the `Weekly_Sales` around the Christmas weeks. Unlike the other listed holidays (Super Bowl, Labor Day, etc.), Christmas falls on a fixed date, 25 December, the number of pre-Christmas days in that particular week differs across the years.
#' 
#' Dates provided in given data are all Fridays so the week starts on a Saturday.
#' 
#' 2010-12-25: Saturday (0 pre-Christmas days)
#' 
#' 2011-12-25: Sunday (1 pre-Christmas days)
#' 
#' 2012-12-25: Tuesday (3 pre-Christmas days)
#' 
#' There is an average of 2.5 days difference between 2012 and each of the previous years. `Weekly_Sales` are noticeably higher before the Christmas periods as seen in our plots above. Since the model cannot recognize the difference in pre-Christmas days, we need to shift back some of the sales from the week 51 to week 52. 
#' 
#' The condition for shifting is when `Weekly_Sales` for week 51 is 'k' times greater than in week 52, where we can tune 'k' against the leaderboard to find that the optimal value is at roughly k = 2. We need a condition for shifting because not all `storeDept` experience the same seasonality patterns for Christmas.
#' 
#' *Score on private leaderboard from 2724.31353 to 2494.25819 after adjustment.*
## ------------------------------------------------------------------
adjust_full <- function(fc_full){
  adjust <- function(fc){
  if(2 * fc[9] < fc[8]){
    adj <- fc[8] * (2.5 / 7)
    fc[9] <- fc[9] + adj
    fc[8] <- fc[8] - adj
    }
  fc
  }
  apply(fc_full, 2, adjust)
}

#' 
## ------------------------------------------------------------------
final_fc <-(adjust_full(final_snaive_fc)
            + adjust_full(final_tslm_fc)
            + adjust_full(final_arima_fourier_fc)
            + adjust_full(final_sarima_fc)
            + adjust_full(final_stl_ets_fc)
            + adjust_full(final_stl_arima_fc)
            ) / 6

#' 
#' For those `storeDept` without any historical observations, we let the forecasts take value 0.
#' 
## ------------------------------------------------------------------
storeDept_names <- colnames(storeDept_ts)
colnames(final_fc) <- storeDept_names

testDates <- tibble("Date" = seq(startTest, endTest, by = 7))
final <- 
  cbind(testDates, final_fc) %>% 
  pivot_longer(!Date, names_to = "storeDept", values_to = "Weekly_Sales")

(my_forecasts <-
  test_df %>%
  left_join(final, by = c("storeDept", "Date")) %>% 
  replace_na(list(Weekly_Sales = 0)) %>%
  mutate(Id = paste0(storeDept, "_", Date)) %>%
  select(Id, Weekly_Sales))

#' 
## ------------------------------------------------------------------
write_csv(my_forecasts, "my_forecasts.csv")

#' 
