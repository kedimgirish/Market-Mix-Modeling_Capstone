GA_Koyck_Model <- function(CE_GA_NPS_Koyck)
{
  
  ## Setting seed to achieve reproducibility
  set.seed(1000)
  
  ## seperate the Training and test datasets
  trainindices = sample(1:nrow(CE_GA_NPS_Koyck),0.7*nrow(CE_GA_NPS_Koyck))
  CE_GA_Koyck_training = CE_GA_NPS_Koyck[trainindices,]
  CE_GA_Koyck_test = CE_GA_NPS_Koyck[-trainindices,]
  View(CE_GA_Koyck_training)
  
  # Build model 1 containing all variables
  GA_Koyck_model_1 <-lm(gmv~.,data=CE_GA_Koyck_training)
  summary(GA_Koyck_model_1)
  
  #Multiple R-squared:  0.996,    Adjusted R-squared:  0.989
  
  # In stepAIC function, we pass our first model i.e model_1 and
  # direction is set as both, because in stepwise,  both the forward selection
  # of variables and backward elimination of variables happen simultaneously.
  
  stepAIC_Koyck_model_1 <- stepAIC(GA_Koyck_model_1, direction = "both")
  stepAIC_Koyck_model_1
  
  Model_2 <- lm(formula = gmv ~ units + product_mrp + sla + list_price + TVAdstock + 
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  OnlineMediaAdstock + AffiliatesAdstock + RadioAdstock + OtherAdstock + 
                  sale_frequency + gmv_lag_1 + gmv_lag_2, data = CE_GA_Koyck_training)
  summary(Model_2)
  # Multiple R-squared:     1,	Adjusted R-squared:  0.999 
  sort(vif(Model_2))
  
  #Removing gmv_lag_1, gmv_lag_2, AffiliatesAdstock due to high vif and p values 
  
  Model_3 <- lm(formula = gmv ~ units + product_mrp + sla + list_price + TVAdstock + 
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  OnlineMediaAdstock + RadioAdstock + OtherAdstock + 
                  sale_frequency , data = CE_GA_Koyck_training)
  summary(Model_3)
  #Multiple R-squared: 0.994,    Adjusted R-squared: 0.992 
  sort(vif(Model_3))
  
  #Removing TVAdstock and OnlineMediaAdstock due to high p value   
  
  Model_4 <- lm(formula = gmv ~ units + product_mrp + sla + list_price +  
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  RadioAdstock + OtherAdstock + sale_frequency, data = CE_GA_Koyck_training)
  summary(Model_4)
  #Multiple R-squared:0.993  ,    Adjusted R-squared: 0.99 
  sort(vif(Model_4))
  
  #Removing sale_frequency due to the p values and vif values 
  Model_5 <- lm(formula = gmv ~ units + product_mrp + sla + list_price +  
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  RadioAdstock + OtherAdstock, data = CE_GA_Koyck_training)
  summary(Model_5)
  #Multiple R-squared: 0.993,	Adjusted R-squared:  0.99 
  sort(vif(Model_5))
  
  
  ## As all the p values are below 0.05, we stop modelling here.
  Final_Koyck_GA <- Model_5 
  library(DAAG)
  
  cv_lm <- cv.lm(data = CE_GA_Koyck_training, form.lm = Final_Koyck_GA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
  ms_GA <- attr(cv_lm,"ms")
  
  # predicting the results in test dataset
  Predict_1 <- predict(Final_Koyck_GA,CE_GA_Koyck_test)
  CE_GA_Koyck_test$test_gmv <- Predict_1
  
  ##Now, we need to test the r square between actual and predicted sales.
  r <- cor(CE_GA_Koyck_test$gmv,CE_GA_Koyck_test$test_gmv)
  rsquared <- cor(CE_GA_Koyck_test$gmv,CE_GA_Koyck_test$test_gmv)^2
  rsquared##0.875
  
  return(list(coefficients(Final_Koyck_GA),summary(Final_Koyck_GA)$r.squared,rsquared,ms_GA))
}