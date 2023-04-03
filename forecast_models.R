#models issue with residuals: glment, svm-poly, svm-rbf, meanf, snaive, croston, nnetar, tbats(errors vs residuals could just be wording)
#deep learning cannot run 'deepar','nbeats', 'tabnet'

devtools::load_all()
library(dplyr)

#Fake Historical Data from Timetk package
 the_data <- timetk::m4_monthly %>%
 dplyr::rename(Date = date) %>%
   dplyr::mutate(id = as.character(id))%>%
   dplyr::filter(id=="M1")
               Date > "1999-12-01")

#Three different tests/sources of data included below

#Surface Data
the_data<- as_tibble(read.csv("surface_data.csv"))%>%
  mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
  arrange(Date)

#Commercial and Enterprise Data
# the_data<- as_tibble(read.csv("c_e_data.csv"))%>%
#   mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
#   arrange(Date)

#Office Data
# the_data<- as_tibble(read.csv("office_data.csv"))%>%
#   mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
#   arrange(Date)


models_run = c('stlm-ets','stlm-arima','theta','mars','arima','croston','ets','prophet')
#models_run = c('mars','arima','theta')
#models_run = c('arima','mars')
#models_run = c('arima','theta')
#models_run= c('mars','theta')

#call to finnts
typefinn_output <- forecast_time_series(
  input_data = the_data,
  combo_variables = c("id"),
  target_variable = "value",
  date_type = "month",
  forecast_horizon = 24,
  back_test_scenarios = NULL, 
  models_to_run = models_run, 
  run_global_models = FALSE, 
  run_model_parallel = TRUE
)


library(quadprog)
library(ForecastComb)
library(forecast)

modeloutputR1 <- readRDS('modeloutputR1.rds')

#Process below:
#get actuals (1st input to foreccomb)

actuals_data_vector <- the_data %>% pull(value)%>% as.numeric()

# find model id # in loop 
didOne=FALSE
count = 1
numAdded <- 1

follow <- 1 
for(model in models_run){
  if(model == "mars")
    models_run[follow]="mars-R1"
  follow = follow + 1
}

for(m in models_run){
  
  notFound <- TRUE
  
  while(notFound){
    if(modeloutputR1[[3]][[count]] == m){
      model_id = count
      notFound = FALSE
    }
    count = count + 1
  }
  
  count = 1
  
  if(m == 'arima' || m == 'ets'|| m == 'theta'){
    residuals_vector_model <- modeloutputR1[[2]][[model_id]][["fit"]][["fit"]][["fit"]][["models"]][["model_1"]][["residuals"]]
  }
  else if(m == 'croston' || m == 'prophet'|| m == 'stlm-arima'|| m == 'stlm-ets'){
    residuals_vector_model <- modeloutputR1[[2]][[model_id]][["fit"]][["fit"]][["fit"]][["data"]][[".residuals"]]
  }
  else if(m == 'mars-R1'){
    residuals_vector_model <- modeloutputR1[[2]][[model_id]][["fit"]][["fit"]][["fit"]][["residuals"]]
  }

  vector_forecasts <- actuals_data_vector - residuals_vector_model

  if(didOne == FALSE){
    df_models <- as.data.frame(vector_forecasts)
    didOne=TRUE
  }
  else{
    temp_model<- as.data.frame(vector_forecasts)
    df_models<- cbind(df_models,temp_model)
  }
  colnames(df_models)[numAdded] <- modeloutputR1$.model_desc[model_id]
  numAdded = numAdded + 1 
}

pred_matrix <- as.matrix(df_models)
test_matrix <- cbind(pred_matrix,actuals_data_vector)


#optimization required
for(r in 1:nrow(pred_matrix)){
  for(c in 1:ncol(pred_matrix)){
    if(is.na(pred_matrix[r,c])){
      pred_matrix[r,c] = test_matrix[r,ncol(test_matrix)]
    }
  }
}

data_length<- as.numeric(nrow(pred_matrix))
train_percent<- ceiling(data_length*.4)

train.obs <- actuals_data_vector[1:train_percent]
train.pred <- pred_matrix[1:train_percent,]
test.obs <- actuals_data_vector[(train_percent + 1):data_length]
test.pred <- pred_matrix[(train_percent + 1) :data_length,]

#call to foreccomb function; documentation available online
input_data <- foreccomb(actuals_data_vector, pred_matrix)
#input_data <- foreccomb(train.obs, train.pred, test.obs, test.pred)

#cs_dispersion(input_data, measure = "SD", plot = TRUE)

# Simple Forecast Combination Functions
BG<- comb_BG(input_data)
inv_rank<-comb_InvW(input_data)
med_for<-comb_MED(input_data)
new_grang<-comb_NG(input_data)
SA<- comb_SA(input_data)
TA<-comb_TA(input_data)
wins<-comb_WA(input_data)

#Regression Based Forecast combinations
OLS_static<- comb_OLS(input_data)
#CLS_static<- comb_CLS(input_data)
CSR<- comb_CSR(input_data)
LAD<- comb_LAD(input_data)

#Eigenvector based forecast combinations. no need to run all
EIG1_static<- comb_EIG1(input_data)
EIG2<- comb_EIG2(input_data)
EIG3<- comb_EIG3(input_data)
EIG4_static<- comb_EIG4(input_data)


#Summary of each combinations
# sum_matrix<-as.data.frame(BG$Accuracy_Test[5])
# sum_matrix<- cbind(sum_matrix, inv_rank$Accuracy_Test[5], new_grang$Accuracy_Test[5],
#                    med_for$Accuracy_Test[5], SA$Accuracy_Test[5], TA$Accuracy_Test[5],
#                    wins$Accuracy_Test[5], OLS_static$Accuracy_Test[5], LAD$Accuracy_Test[5],
#                    EIG1_static$Accuracy_Test[5], EIG2$Accuracy_Test[5],
#                    EIG3$Accuracy_Test[5], EIG4_static$Accuracy_Test[5],CSR$Accuracy_Test[5],
#                    CSR$Accuracy_Test[10],CSR$Accuracy_Test[15],CSR$Accuracy_Test[20])

sum_matrix<-as.data.frame(BG$Accuracy_Train[5])
sum_matrix<- cbind(sum_matrix, inv_rank$Accuracy_Train[5], new_grang$Accuracy_Train[5],
                   med_for$Accuracy_Train[5], SA$Accuracy_Train[5], TA$Accuracy_Train[5],
                   wins$Accuracy_Train[5], OLS_static$Accuracy_Train[5], LAD$Accuracy_Train[5],
                   EIG1_static$Accuracy_Train[5], EIG2$Accuracy_Train[5],
                   EIG3$Accuracy_Train[5], EIG4_static$Accuracy_Train[5],CSR$Accuracy_Train[5],
                   CSR$Accuracy_Train[10],CSR$Accuracy_Train[15],CSR$Accuracy_Train[20])


sum_matrix[which.min(sum_matrix)]
summary(LAD)
CSR$Accuracy_Train
plot(LAD)

# summary(med_for)
summary(SA)
# summary(TA)
# summary(wins)
# summary(OLS_static)
# CSR$Accuracy_Test[20]
# summary(LAD)
# summary(EIG1_static)
# summary(EIG2)
# summary(EIG3)
# summary(EIG4_static)
