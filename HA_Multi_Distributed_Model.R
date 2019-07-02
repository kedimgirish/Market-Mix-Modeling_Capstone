###########Multiplicative + Distributed Model##########
#######HA#####


HA_Multi_Distributed_Model <- function(CE_CA_Final)
{
  
  CE_HA_lagvariable_Final <-lagVariable_Gmv(CE_HA_Final)
  View(CE_HA_lagvariable_Final)
  CE_HA_lagvariable_Final$Moving_Average <- NULL
  
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
  CE_HA_lagvariable_Final[CE_HA_lagvariable_Final == 0] <- 0.00001
  
  ## Tranforming the negative values
  CE_HA_lagvariable_Final$priceInf <- 1 + CE_HA_lagvariable_Final$priceInf - min(CE_HA_lagvariable_Final$priceInf)
  #View(CE_HA_lagvariable_Final)
  
  CE_HA_lagvariable_Final$LP_lag_1 <- 1 + CE_HA_lagvariable_Final$LP_lag_1 - min(CE_HA_lagvariable_Final$LP_lag_1)
  CE_HA_lagvariable_Final$LP_lag_2 <- 1 + CE_HA_lagvariable_Final$LP_lag_2 - min(CE_HA_lagvariable_Final$LP_lag_2)
  CE_HA_lagvariable_Final$LP_lag_3 <- 1 + CE_HA_lagvariable_Final$LP_lag_3 - min(CE_HA_lagvariable_Final$LP_lag_3)
  
  CE_HA_lagvariable_Final$gmv_lag_1 <- 1 + CE_HA_lagvariable_Final$gmv_lag_1 - min(CE_HA_lagvariable_Final$gmv_lag_1)
  CE_HA_lagvariable_Final$gmv_lag_2 <- 1 + CE_HA_lagvariable_Final$gmv_lag_2 - min(CE_HA_lagvariable_Final$gmv_lag_2)
  CE_HA_lagvariable_Final$gmv_lag_3 <- 1 + CE_HA_lagvariable_Final$gmv_lag_3 - min(CE_HA_lagvariable_Final$gmv_lag_3)
  #View(CE_HA_lagvariable_Final)
  CE_HA_Multi_Distributed <- log(CE_HA_lagvariable_Final)
  #View(CE_GA_Multi_Distributed)
  
  #####BUILDING A MODEL ON CE_GA_Multi_Distributed dataset#########
  
  ## Setting seed to achieve reproducibility
  set.seed(1000)
  
  ## seperate the Training and test datasets
  trainindices = sample(1:nrow(CE_HA_Multi_Distributed),0.7*nrow(CE_HA_Multi_Distributed))
  CE_HA_Multi_Distributed_training = CE_HA_Multi_Distributed[trainindices,]
  CE_HA_Multi_Distributed_test = CE_HA_Multi_Distributed[-trainindices,]
  
  # Build model 1 containing all variables
  HA_Multi_Distributed_model_1 <-lm(gmv~.,data=CE_HA_Multi_Distributed_training)
  summary(HA_Multi_Distributed_model_1)
  
  # Multiple R-squared:1      ,	Adjusted R-squared:1 
  
  # In stepAIC function, we pass our first model i.e GA_Multi_Distributed_model_1 and 
  # direction is set as both, because in stepwise,  both the forward selection 
  # of variables and backward elimination of variables happen simultaneously.
  
  stepAIC_Multi_Distributed_model_1 <- stepAIC(HA_Multi_Distributed_model_1, direction = "both")
  stepAIC_Multi_Distributed_model_1
  
  # Build Model 2
  HA_Multi_Distributed_model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                                       product_procurement_sla + list_price + per_order + TVAdstock + 
                                       DigitalAdstock + ContentMediaAdstock + OnlineMediaAdstock + 
                                       AffiliatesAdstock + RadioAdstock + OtherAdstock + sale_frequency + 
                                       isSaleWeek + NPS + LP_lag_1 + priceInf, data = CE_HA_Multi_Distributed_training)
  
  
  summary(HA_Multi_Distributed_model_2)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(HA_Multi_Distributed_model_2)
 
  #week_updated,RadioAdstock, units,TVAdstock, OtherAdstock, product_mrp,sale_frequency,ContentMediaAdstock,isSaleWeek,product_procurement_sla,OnlineMediaAdstock
  # list_price and AffiliatesAdstock have high vif
  # check the p value for these variables
  # RadioAdstock,TVAdstock, OtherAdstock, sale_frequency,isSaleWeek, product_procurement_sla and AffiliatesAdstock are insignificant
  
  # Build Model 3
  HA_Multi_Distributed_model_3 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                                       list_price + per_order + 
                                       DigitalAdstock + ContentMediaAdstock + OnlineMediaAdstock + 
                                       NPS + LP_lag_1 + priceInf, data = CE_HA_Multi_Distributed_training)
  
  
  summary(HA_Multi_Distributed_model_3)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(HA_Multi_Distributed_model_3)
  #week_updated, ContentMediaAdstock,units,OnlineMediaAdstock, product_mrp, list_price,priceInf have high vif
  # check the p value for these variables
  # priceInf is insignificant , hence removing it
  
  
  # Build Model 4
  HA_Multi_Distributed_model_4 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                                       list_price + per_order + 
                                       DigitalAdstock + ContentMediaAdstock + OnlineMediaAdstock + 
                                       NPS + LP_lag_1 , data = CE_HA_Multi_Distributed_training)
  
  
  summary(HA_Multi_Distributed_model_4)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(HA_Multi_Distributed_model_4)
  # week_updated, ContentMediaAdstock, units,OnlineMediaAdstock, product_mrp and list_price have high vif
  # p value for these variables are significant
  # Lets remove the insignificant variables
  # per_order,DigitalAdstock,NPS and LP_lag_1 are insignificant, hence removing them
  
  # Build Model 5
  HA_Multi_Distributed_model_5 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                                       list_price +  
                                       ContentMediaAdstock + OnlineMediaAdstock, 
                                      data = CE_HA_Multi_Distributed_training)
  
  
  summary(HA_Multi_Distributed_model_5)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(HA_Multi_Distributed_model_5)
  
  # All the variables look significant. Hence we can stop the modeling
  
  Final_Multi_Distributed_model_HA <- HA_Multi_Distributed_model_5
  library(DAAG)
  cv_lm <- cv.lm(data = CE_HA_Multi_Distributed_training, form.lm = Final_Multi_Distributed_model_HA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
  ms_HA <- attr(cv_lm,"ms")
  
  # predicting the results in test dataset
  Predict_1 <- predict(Final_Multi_Distributed_model_HA,CE_HA_Multi_Distributed_test)
  CE_HA_Multi_Distributed_test$test_gmv <- Predict_1
  
  ##Now, we need to test the r square between actual and predicted sales. 
  r <- cor(CE_HA_Multi_Distributed_test$gmv,CE_HA_Multi_Distributed_test$test_gmv)
  rsquared <- cor(CE_HA_Multi_Distributed_test$gmv,CE_HA_Multi_Distributed_test$test_gmv)^2
  rsquared
  
  source("elasticity.R")
  elasticityFun(CE_HA_Multi_Distributed,Final_Multi_Distributed_model_HA,"Home Accessory Elasticity Plot")
  
  return(list(coefficients(Final_Multi_Distributed_model_HA),summary(Final_Multi_Distributed_model_HA)$r.squared,rsquared,ms_HA))
}
