CA_Koyck_Model <- function(CE_CA_NPS_Koyck)
{
  ## Setting seed to achieve reproducibility
  set.seed(1000)
  
  ## seperate the Training and test datasets
  trainindices = sample(1:nrow(CE_CA_NPS_Koyck),0.7*nrow(CE_CA_NPS_Koyck))
  CE_CA_Koyck_training = CE_CA_NPS_Koyck[trainindices,]
  CE_CA_Koyck_test = CE_CA_NPS_Koyck[-trainindices,]
  
  
  # Build model 1 containing all variables
  CA_Koyck_model_1 <-lm(gmv~.,data=CE_CA_Koyck_training)
  summary(CA_Koyck_model_1)
  
  #Multiple R-squared:  0.998,    Adjusted R-squared:  0.995
  
  # In stepAIC function, we pass our first model i.e model_1 and
  # direction is set as both, because in stepwise,  both the forward selection
  # of variables and backward elimination of variables happen simultaneously.
  
  stepAIC_Koyck_model_1 <- stepAIC(CA_Koyck_model_1, direction = "both")
  stepAIC_Koyck_model_1
  
  Model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + product_procurement_sla + 
                  list_price + discount + DigitalAdstock + SponsorshipAdstock + 
                  OnlineMediaAdstock + AffiliatesAdstock + SEMAdstock + RadioAdstock + 
                  OtherAdstock + sale_frequency + NPS, data = CE_CA_Koyck_training)
  
  summary(Model_2)
  #Multiple R-squared:  0.998,    Adjusted R-squared:  0.997
  sort(vif(Model_2))
  ### Removing SEMAdstock and DigitalAdstock as the vif 368.42 and 266.00 
  Model_3 <- lm(formula = gmv ~ week_updated + units + product_mrp + product_procurement_sla + 
                  list_price + discount +  SponsorshipAdstock + 
                  OnlineMediaAdstock + AffiliatesAdstock + RadioAdstock + 
                  OtherAdstock + sale_frequency + NPS, data = CE_CA_Koyck_training)
  
  summary(Model_3)
  sort(vif(Model_3))
  #Multiple R-squared:  0.998,    Adjusted R-squared:  0.996
  
  ##Removing RadioAdstock as the vif value is 109.81
  Model_4 <- lm(formula = gmv ~ week_updated + units + product_mrp + product_procurement_sla + 
                  list_price + discount +  SponsorshipAdstock + 
                  OnlineMediaAdstock + AffiliatesAdstock +  
                  OtherAdstock + sale_frequency + NPS, data = CE_CA_Koyck_training)
  
  summary(Model_4)
  #Multiple R-squared:  0.997,    Adjusted R-squared:  0.996
  sort(vif(Model_4))
  ##Removing units as the vif value is 165.22
  Model_5 <- lm(formula = gmv ~ week_updated + product_mrp + product_procurement_sla + 
                  list_price + discount +  SponsorshipAdstock + 
                  OnlineMediaAdstock + AffiliatesAdstock +  
                  OtherAdstock + sale_frequency + NPS, data = CE_CA_Koyck_training)
  
  summary(Model_5)
  #Multiple R-squared:0.997  ,    Adjusted R-squared:0.996 
  sort(vif(Model_5))
  
  ##Removing NPS,SponsorshipAdstock due to high vif and p value
  Model_6 <- lm(formula = gmv ~ week_updated + product_mrp + product_procurement_sla + 
                  list_price + discount +  
                  OnlineMediaAdstock + AffiliatesAdstock +  
                  OtherAdstock + sale_frequency , data = CE_CA_Koyck_training)
  
  summary(Model_6)
  #Multiple R-squared:0.997  ,    Adjusted R-squared:0.996 
  sort(vif(Model_6))
  
  ##Removing OnlineMediaAdstock due to high vif of 98.41
  Model_7 <- lm(formula = gmv ~ week_updated + product_mrp + product_procurement_sla + 
                  list_price + discount + AffiliatesAdstock +  
                  OtherAdstock + sale_frequency , data = CE_CA_Koyck_training)
  
  summary(Model_7)
  #Multiple R-squared:0.996  ,    Adjusted R-squared:0.995 
  sort(vif(Model_7))
  ##Removing AffiliatesAdstock due to high p value of 0.387 and sale_frequency due to p value of 0.11
  Model_8 <- lm(formula = gmv ~ week_updated + product_mrp + product_procurement_sla + 
                  list_price + discount +  
                  OtherAdstock , data = CE_CA_Koyck_training)
  
  summary(Model_8)
  #Multiple R-squared:0.996  ,    Adjusted R-squared:0.995 
  sort(vif(Model_8))
  ##Removing product_procurement_sla and OtherAdstock due to high p value of 0.0136 and 0.0116 respectively 
  Model_9 <- lm(formula = gmv ~ week_updated + product_mrp +  
                  list_price + discount , data = CE_CA_Koyck_training)
  
  summary(Model_9)
  #Multiple R-squared: 0.994  ,    Adjusted R-squared: 0.993
  sort(vif(Model_9))
  
  ##Removing list_pricec due to high p value of 0.0107 
  Model_10 <- lm(formula = gmv ~ week_updated + product_mrp + discount , data = CE_CA_Koyck_training)
  
  summary(Model_10)
  #Multiple R-squared: 0.993  ,    Adjusted R-squared: 0.993
  sort(vif(Model_10))
  
  ## As all the p values are below 0.05, we stop modelling here.
  Final_Koyck_CA <- Model_10
  library(DAAG)
  cv_lm <- cv.lm(data = CE_CA_Koyck_training, form.lm = Final_Koyck_CA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
  ms_CA <- attr(cv_lm,"ms")
  
  # predicting the results in test dataset
  Predict_1 <- predict(Final_Koyck_CA,CE_CA_Koyck_test)
  CE_CA_Koyck_test$test_gmv <- Predict_1
  
  ##Now, we need to test the r square between actual and predicted sales.
  r <- cor(CE_CA_Koyck_test$gmv,CE_CA_Koyck_test$test_gmv)
  rsquared <- cor(CE_CA_Koyck_test$gmv,CE_CA_Koyck_test$test_gmv)^2
  rsquared## 0.932
  
  return(list(coefficients(Final_Koyck_CA),summary(Final_Koyck_CA)$r.squared,rsquared,ms_CA))
}
