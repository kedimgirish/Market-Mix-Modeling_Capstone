CA_Distributed_Lag_Regression <- function(CE_CA_NPS_lag_Distributed_Lag)
{
  ## Setting seed to achieve reproducibility
  set.seed(1000)
  
  ## seperate the Training and test datasets
  trainindices = sample(1:nrow(CE_CA_NPS_lag_Distributed_Lag),0.7*nrow(CE_CA_NPS_lag_Distributed_Lag))
  CE_CA_training = CE_CA_NPS_lag_Distributed_Lag[trainindices,]
  CE_CA_test = CE_CA_NPS_lag_Distributed_Lag[-trainindices,]
  
  
  # Build model 1 containing all variables
  CA_model_1 <-lm(gmv~.,data=CE_CA_training)
  summary(CA_model_1)
  #Multiple R-squared:  0.998,	Adjusted R-squared:  0.993 
  
  
  stepAIC_model_1 <- stepAIC(CA_model_1, direction = "both")
  stepAIC_model_1
  
  Model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + product_procurement_sla + 
                  list_price + discount + DigitalAdstock + SponsorshipAdstock + 
                  OnlineMediaAdstock + AffiliatesAdstock + SEMAdstock + RadioAdstock + 
                  OtherAdstock + sale_frequency + LP_lag_1 + gmv_lag_1 + gmv_lag_2 + 
                  gmv_lag_3, data = CE_CA_training)
  summary(Model_2)
  #Multiple R-squared:  0.998,	Adjusted R-squared:  0.996 
  sort(vif(Model_2))
  
  
  Model_3 <- lm(formula = gmv ~ week_updated + product_mrp + product_procurement_sla + 
                  SponsorshipAdstock + 
                  OnlineMediaAdstock + AffiliatesAdstock  + RadioAdstock + 
                  OtherAdstock 
                  , data = CE_CA_training)
  summary(Model_3)
  #Multiple R-squared:  0.978,	Adjusted R-squared:  0.971 
  vif(Model_3)
  
  
  Model_4 <- lm(formula = gmv ~ product_mrp 
                , data = CE_CA_training)
  summary(Model_4)
  #Multiple R-squared:  0.975,	Adjusted R-squared:  0.974 
  vif(Model_4)
  
  
  ## As all the p values are below 0.05, we stop modelling here. 
  Final_Distributed_Lag_model_CA <- Model_4
  # predicting the results in test dataset
  Predict_1 <- predict(Final_Distributed_Lag_model_CA,CE_CA_test)
  CE_CA_test$test_gmv <- Predict_1
  
  ##Now, we need to test the r square between actual and predicted sales. 
  r <- cor(CE_CA_test$gmv,CE_CA_test$test_gmv)
  rsquared <- cor(CE_CA_test$gmv,CE_CA_test$test_gmv)^2
  rsquared##0.997
  
  crsValildation <- cv.lm(CE_CA_training,Final_Distributed_Lag_model_CA,m=4)
  
  msValue <- attr(crsValildation,"ms")#114331675888
  
  return(list(coefficients(Final_Distributed_Lag_model_CA),summary(Final_Distributed_Lag_model_CA)$r.squared,rsquared,msValue))
}