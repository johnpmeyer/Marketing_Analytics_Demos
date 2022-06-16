library(readr)
library(tidyverse)

# This script has three sections.
# 
# Section 1 -- Load the data from a dataframe and split into test and training groups.
# Section 2 -- Test five different models including month and spend as a factor to predict registrations 
# Section 3 -- Use a seasonal index to deseasonalize data, predict registrations based on spend. 
# Section 4 -- Take SSE of each model's prediction on the test data set to determine which predictive model to use.


#Section 1 -- Loading Data ----

#load monthly data. note, this data has 3 columns:
# month -- the month of a registration
# spend -- spend in the given month
# reg   -- registrations attributed to spend in analytics.
data = read_csv("cost per hs reg.csv")

#add cost_per_reg
data_cost_reg = data %>%
  mutate(cost_reg = spend / registrations)

#split into test and training data. 
#First 3 years are training data.
#Jan - May 2022 are test data.

train = data[1:36, ]
test = data[37:41, ]

test_predictors = test[, 2]
test_predictors_month = test[, 1:2]

#Section 2 -- Simple, non-seasonal adjusted models ----

#model1 = log of response = month and spend
#model2 = log of response = month
#model3 = response = log(spend) + month
#model4 = response = log(spend)
#model5 = log(response) = log(spend) + month

#log linear with month
model1 = lm(log(registrations) ~ month + spend, data = train)
summary(model1)

preds = predict(model1, test_predictors_month)
preds_model1 = exp(preds)
sse_model1 = sqrt(sum((preds_model1 - test[, 3])**2))

#log linear w.o. month
model2 = lm(log(registrations) ~ spend, data = train)
summary(model2)

preds = predict(model2, test_predictors)
preds_model2 = exp(preds)
sse_model2 = sqrt(sum((preds_model2 - test[, 3])**2))

#linear log
model3 = lm(registrations ~ month + log(spend), data = train)
summary(model3)

preds_model3 = predict(model3, test_predictors_month)
sse_model3 = sqrt(sum((preds_model3 - test[, 3])**2))

#linear log w.o month
model4 = lm(registrations ~ log(spend), data = train)
summary(model4)

preds_model4 = predict(model4, test_predictors)
sse_model4 = sqrt(sum((preds_model4 - test[, 3])**2))

#log log with month
model5 = lm(log(registrations) ~ log(spend) + month, data = train)

preds_model5 = predict(model5, test_predictors_month)
preds_model5 = exp(preds_model5)
sse_model5 = sqrt(sum((preds_model5 - test[, 3])**2))


#Section 3 -- Seasonally-adjusted models ----

make_seasonal_adjustment = function(adj) {
  
  #Note: this is dummy monthly data. For the real process, I determined this through Holt-Winters Multiplicative Modeling.
  
  seasonal = c(0.9, 0.85, 0.98,
               1.01, 0.65, 0.69, 
               0.60, 1.65, 1.92, 
               0.98, 1.11, 0.99)
  
  seasonal_adj = ((seasonal - 1) * adj) + 1
  
  seasonal = rep(seasonal_adj, 3)
  
  data = cbind(train, seasonal)
  
  data_cost_reg = data %>%
    mutate(adj_reg = registrations / seasonal,
           cost_reg = spend / registrations,
           adj_cost_reg = spend / adj_reg)
  
  return(data_cost_reg)
  
}

#overall, this takes our data and deseasonalizes it by dividing registrations
#by their seasonal index. Seasonal index is dampened by the 'adj' parameter.

adj_normal = make_seasonal_adjustment(1)
adj_75 = make_seasonal_adjustment(.75)
adj_50 = make_seasonal_adjustment(.5)

#log linear, seasonal adjustment
model6 = lm(log(adj_reg) ~ spend, data = adj_normal)
summary(model6)

preds_model6 = predict(model6, test_predictors)
preds_model6 = exp(preds_model6)
preds_model6 = preds_model6 * adj_normal$seasonal[1:5]
sse_model6 = sqrt(sum((preds_model6 - test[, 3])**2))

#linear log, seasonal adjustment
model7 = lm(adj_reg ~ log(spend), data = adj_normal)
summary(model7)

preds_model7 = predict(model7, test_predictors)
preds_model7 = preds_model7 * adj_normal$seasonal[1:5]
sse_model7 = sqrt(sum((preds_model7 - test[, 3])**2))

#log linear, seasonal adjustment 0.75
model8 = lm(log(adj_reg) ~ spend, data = adj_75)
summary(model8)

preds_model8 = predict(model8, test_predictors)
preds_model8 = exp(preds_model8)
preds_model8 = preds_model8 * adj_normal$seasonal[1:5]
sse_model8 = sqrt(sum((preds_model8 - test[, 3])**2))

#linear log, seasonal adjustment 0.75
model9 = lm(adj_reg ~ log(spend), data = adj_75)
summary(model9)

preds_model9 = predict(model9, test_predictors)
preds_model9 = preds_model9 * adj_normal$seasonal[1:5]
sse_model9 = sqrt(sum((preds_model9 - test[, 3])**2))


#log linear, seasonal adjustment 0.50
model10 = lm(log(adj_reg) ~ spend, data = adj_50)
summary(model10)

preds_model10 = predict(model10, test_predictors)
preds_model10 = exp(preds_model10)
preds_model10 = preds_model10 * adj_normal$seasonal[1:5]
sse_model10 = sqrt(sum((preds_model10 - test[, 3])**2))

#linear log, seasonal adjustment 0.50
model11 = lm(adj_reg ~ log(spend), data = adj_50)
summary(model11)

preds_model11 = predict(model11, test_predictors)
preds_model11 = preds_model11 * adj_normal$seasonal[1:5]
sse_model11 = sqrt(sum((preds_model11 - test[, 3])**2))



# Summaries ---- 

##taking log of response + month
sse_model1

##taking log of response
sse_model2

#taking log of predictor + month
sse_model3

#taking log of predictor
sse_model4

#log log with month
sse_model5

#seasonal adjusters - log-linear full
sse_model6

#seasonal adjusters - linear-log full
sse_model7

#seasonal adjusters - log-linear 0.75
sse_model8

#seasonal adjusters  - linear-log 0.75
sse_model9

#seasonal adjusters  - log-linear 0.5
sse_model10

#seasonal adjusters  - linear-log 0.5
sse_model11

final_df = data.frame(actuals = test$registrations,
                      model1 = preds_model1,
                      model2 = preds_model2,
                      model3 = preds_model3,
                      model4 = preds_model4,
                      model5 = preds_model5,
                      model6 = preds_model6,
                      model7 = preds_model7,
                      model8 = preds_model8,
                      model9 = preds_model9,
                      model10 = preds_model10,
                      model11 = preds_model11)

sse = data.frame(actuals = 0,
                 model1 = sse_model1,
                 model2 = sse_model2,
                 model3 = sse_model3,
                 model4 = sse_model4,
                 model5 = sse_model5,
                 model6 = sse_model6,
                 model7 = sse_model7,
                 model8 = sse_model8,
                 model9 = sse_model9,
                 model10 = sse_model10,
                 model11 = sse_model11)

final_df = bind_rows(final_df, sse)
