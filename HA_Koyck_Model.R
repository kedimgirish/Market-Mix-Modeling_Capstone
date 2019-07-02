HA_Koyck_Model <- function(CE_HA_NPS_Koyck)
{
  ## Setting seed to achieve reproducibility
  set.seed(1000)
  
  ## seperate the Training and test datasets
  trainindices = sample(1:nrow(CE_HA_NPS_Koyck),0.7*nrow(CE_HA_NPS_Koyck))
  CE_HA_Koyck_training = CE_HA_NPS_Koyck[trainindices,]
  CE_HA_Koyck_test = CE_HA_NPS_Koyck[-trainindices,]
  
  
  # Build model 1 containing all variables
  HA_Koyck_model_1 <-lm(gmv~.,data=CE_HA_Koyck_training)
  summary(HA_Koyck_model_1)
  
  #Multiple R-squared:  1,    Adjusted R-squared:  0.999
  
  # In stepAIC function, we pass our first model i.e model_1 and
  # direction is set as both, because in stepwise,  both the forward selection
  # of variables and backward elimination of variables happen simultaneously.
  
  stepAIC_Koyck_model_1 <- stepAIC(HA_Koyck_model_1, direction = "both")
  stepAIC_Koyck_model_1
  
  
  Model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                  product_procurement_sla + list_price + discount + DigitalAdstock + 
                  SponsorshipAdstock + ContentMediaAdstock + OnlineMediaAdstock + 
                  AffiliatesAdstock + SEMAdstock + RadioAdstock + OtherAdstock + 
                  sale_frequency + isSaleWeek + NPS + gmv_lag_1 + gmv_lag_2 + 
                  gmv_lag_3, data = CE_HA_Koyck_training)
  summary(Model_2)
  #Multiple R-squared:  1,    Adjusted R-squared:  0.999
  sort(vif(Model_2))
  
  ##Removing OnlineMediaAdstock due to high vif and p value,AffiliatesAdstock, 
  Model_3 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                  product_procurement_sla + list_price + discount + DigitalAdstock + 
                  SponsorshipAdstock +AffiliatesAdstock + SEMAdstock + RadioAdstock + OtherAdstock + 
                  sale_frequency + isSaleWeek + NPS + gmv_lag_1 + gmv_lag_2 + 
                  gmv_lag_3, data = CE_HA_Koyck_training)
  summary(Model_3)
  #Multiple R-squared: 0.999  ,    Adjusted R-squared:0.998
  sort(vif(Model_3))
  
  # Removing SEMAdstock, DigitalAdstock, RadioAdstock, OtherAdstock due to their high vif values 
  Model_4 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                  product_procurement_sla + list_price + discount + 
                  SponsorshipAdstock +AffiliatesAdstock +  
                  sale_frequency + isSaleWeek + NPS + gmv_lag_1 + gmv_lag_2 + 
                  gmv_lag_3, data = CE_HA_Koyck_training)
  summary(Model_4)
  #Multiple R-squared:0.999  ,    Adjusted R-squared:0.998
  sort(vif(Model_4))
  
  ## Removing product_mrp, discount, product_procurement_sla,sale_frequency,AffiliatesAdstock
  Model_5 <-  lm(formula = gmv ~ week_updated + units + sla + list_price +  
                   SponsorshipAdstock+ isSaleWeek + NPS + gmv_lag_1 + gmv_lag_2 + 
                   gmv_lag_3, data = CE_HA_Koyck_training)
  summary(Model_5)
  #Multiple R-squared:0.998  ,    Adjusted R-squared: 0.997 
  sort(vif(Model_5))
  
  # Removing NPS,SponsorshipAdstock,isSaleWeek
  Model_6 <- lm(formula = gmv ~ week_updated + units + sla + list_price + gmv_lag_1 + gmv_lag_2 + 
                  gmv_lag_3, data = CE_HA_Koyck_training)
  summary(Model_6)
  #Multiple R-squared: 0.998  ,    Adjusted R-squared:0.997  
  sort(vif(Model_6))
  
  # Removing gmv_lag_1,gmv_lag_2,gmv_lag_3,sla
  Model_7 <- lm(formula = gmv ~ week_updated + units + list_price, data = CE_HA_Koyck_training)
  summary(Model_7)
  #Multiple R-squared: 0.997  ,    Adjusted R-squared:0.997  
  sort(vif(Model_7))
  
  # Removing week_updated due to high p value
  Model_8 <- lm(formula = gmv ~  units + list_price, data = CE_HA_Koyck_training)
  summary(Model_8)
  #Multiple R-squared: 0.997,    Adjusted R-squared:0.997 
  sort(vif(Model_8))
  
  ## As all the p values are below 0.05, we stop modelling here.
  Final_Koyck_HA <- Model_8
  
  library(DAAG)
  cv_lm <- cv.lm(data = CE_HA_Koyck_training, form.lm = Final_Koyck_HA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
  ms_HA <- attr(cv_lm,"ms")
  
  # predicting the results in test dataset
  Predict_1 <- predict(Final_Koyck_HA,CE_HA_Koyck_test)
  CE_HA_Koyck_test$test_gmv <- Predict_1
  
  ##Now, we need to test the r square between actual and predicted sales.
  r <- cor(CE_HA_Koyck_test$gmv,CE_HA_Koyck_test$test_gmv)
  rsquared <- cor(CE_HA_Koyck_test$gmv,CE_HA_Koyck_test$test_gmv)^2
  rsquared## 0.999
  
  return(list(coefficients(Final_Koyck_HA),summary(Final_Koyck_HA)$r.squared,rsquared,ms_HA))
}


