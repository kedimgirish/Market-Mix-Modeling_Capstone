###########Multiplicative + Distributed Model##########
#######GA#####


GA_Multi_Distributed_Model <- function(CE_GA_Final)
{
 
  CE_GA_lagvariable_Final <-lagVariable_Gmv(CE_GA_Final)
  #View(CE_GA_lagvariable_Final)
  CE_GA_lagvariable_Final$Moving_Average <- NULL
  
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
  CE_GA_lagvariable_Final[CE_GA_lagvariable_Final == 0] <- 0.00001
  
  ## Tranforming the negative values
  CE_GA_lagvariable_Final$priceInf <- 1 + CE_GA_lagvariable_Final$priceInf - min(CE_GA_lagvariable_Final$priceInf)
  #View(CE_GA_lagvariable_Final)
  
  CE_GA_lagvariable_Final$LP_lag_1 <- 1 + CE_GA_lagvariable_Final$LP_lag_1 - min(CE_GA_lagvariable_Final$LP_lag_1)
  CE_GA_lagvariable_Final$LP_lag_2 <- 1 + CE_GA_lagvariable_Final$LP_lag_2 - min(CE_GA_lagvariable_Final$LP_lag_2)
  CE_GA_lagvariable_Final$LP_lag_3 <- 1 + CE_GA_lagvariable_Final$LP_lag_3 - min(CE_GA_lagvariable_Final$LP_lag_3)
  
  CE_GA_lagvariable_Final$gmv_lag_1 <- 1 + CE_GA_lagvariable_Final$gmv_lag_1 - min(CE_GA_lagvariable_Final$gmv_lag_1)
  CE_GA_lagvariable_Final$gmv_lag_2 <- 1 + CE_GA_lagvariable_Final$gmv_lag_2 - min(CE_GA_lagvariable_Final$gmv_lag_2)
  CE_GA_lagvariable_Final$gmv_lag_3 <- 1 + CE_GA_lagvariable_Final$gmv_lag_3 - min(CE_GA_lagvariable_Final$gmv_lag_3)
  View(CE_GA_lagvariable_Final)
  CE_GA_Multi_Distributed <- log(CE_GA_lagvariable_Final)
  #View(CE_GA_Multi_Distributed)
  
  #####BUILDING A MODEL ON CE_GA_Multi_Distributed dataset#########
  
  ## Setting seed to achieve reproducibility
  set.seed(1000)
  
  ## seperate the Training and test datasets
  trainindices = sample(1:nrow(CE_GA_Multi_Distributed),0.7*nrow(CE_GA_Multi_Distributed))
  CE_GA_Multi_Distributed_training = CE_GA_Multi_Distributed[trainindices,]
  CE_GA_Multi_Distributed_test = CE_GA_Multi_Distributed[-trainindices,]
  
  # Build model 1 containing all variables
  GA_Multi_Distributed_model_1 <-lm(gmv~.,data=CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_1)
  
  # Multiple R-squared:1      ,	Adjusted R-squared:1 
  
  # In stepAIC function, we pass our first model i.e GA_Multi_Distributed_model_1 and 
  # direction is set as both, because in stepwise,  both the forward selection 
  # of variables and backward elimination of variables happen simultaneously.
  
  stepAIC_Multi_Distributed_model_1 <- stepAIC(GA_Multi_Distributed_model_1, direction = "both")
  stepAIC_Multi_Distributed_model_1
  
  # Build Model 2
  GA_Multi_Distributed_model_2 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                                   per_order + TVAdstock + DigitalAdstock + SponsorshipAdstock + 
                                   OnlineMediaAdstock + AffiliatesAdstock + OtherAdstock + isSaleWeek + 
                                   LP_lag_1 + LP_lag_2 + LP_lag_3 + priceInf + gmv_lag_2 + gmv_lag_3, 
                                   data = CE_GA_Multi_Distributed_training)
  
  
  summary(GA_Multi_Distributed_model_2)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(GA_Multi_Distributed_model_2)
  # OnlineMediaAdstock, AffiliatesAdstock,TVAdstock,units,SponsorshipAdstock,priceInf,per_order and LP_lag_1 have highest VIF values
  # Check the p value for these variables 
  #priceInf, per_order are insignificant , hence removing those variables
  
  # Build Model 3
  GA_Multi_Distributed_model_3 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                                       TVAdstock + DigitalAdstock + SponsorshipAdstock + 
                                       OnlineMediaAdstock + AffiliatesAdstock + OtherAdstock + isSaleWeek + 
                                       LP_lag_1 + LP_lag_2 + LP_lag_3 + gmv_lag_2 + gmv_lag_3, 
                                     data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_3)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(GA_Multi_Distributed_model_3)
  #units,OnlineMediaAdstock,LP_lag_3,AffiliatesAdstock,list_price,TVAdstock,LP_lag_1 and SponsorshipAdstock have high vif
  # check the p value for all these variables
  #SponsorshipAdstock is insignificant, hence removing it
  
  #Build Model 4
  GA_Multi_Distributed_model_4 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                                       TVAdstock + DigitalAdstock +  
                                       OnlineMediaAdstock + AffiliatesAdstock + OtherAdstock + isSaleWeek + 
                                       LP_lag_1 + LP_lag_2 + LP_lag_3 + gmv_lag_2 + gmv_lag_3, 
                                     data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_4)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(GA_Multi_Distributed_model_4)
  # units,AffiliatesAdstock,gmv_lag_2,list_price,TVAdstock,LP_lag_1,DigitalAdstock,OnlineMediaAdstock and LP_lag_3 have high vif
  # check the p value for these variables
  # LP_lag_3 is insignificant, hence removing it
  
  #Build Model 5
  GA_Multi_Distributed_model_5 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                                       TVAdstock + DigitalAdstock +  
                                       OnlineMediaAdstock + AffiliatesAdstock + OtherAdstock + isSaleWeek + 
                                       LP_lag_1 + LP_lag_2 + gmv_lag_2 + gmv_lag_3, 
                                     data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_5)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  
  vif(GA_Multi_Distributed_model_5)
  #units,AffiliatesAdstock,product_procurement_sla,OtherAdstock,list_price,TVAdstock,LP_lag_1,DigitalAdstock,LP_lag_2 and OnlineMediaAdstock have high vif
  # check the p value for these variables
  # product_procurement_sla, OtherAdstock are insignificant, hence removing
  
  #Build Model 6
  GA_Multi_Distributed_model_6 <- lm(formula = gmv ~ units + list_price + 
                                       TVAdstock + DigitalAdstock +  
                                       OnlineMediaAdstock + AffiliatesAdstock + isSaleWeek + 
                                       LP_lag_1 + LP_lag_2 + gmv_lag_2 + gmv_lag_3, 
                                     data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_6)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(GA_Multi_Distributed_model_6)
  # units,LP_lag_1,list_price,LP_lag_2,TVAdstock,DigitalAdstock,OnlineMediaAdstock, AffiliatesAdstock have high vif
  # check the p value for these variables
  # LP_lag_1 is insignificant, hence removing
  
  #Build Model 7
  GA_Multi_Distributed_model_7<- lm(formula = gmv ~ units + list_price + 
                                       TVAdstock + DigitalAdstock +  
                                       OnlineMediaAdstock + AffiliatesAdstock + isSaleWeek + 
                                       LP_lag_2 + gmv_lag_2 + gmv_lag_3, 
                                     data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_7)
  #Multiple R-squared:      1,	Adjusted R-squared:      1
  vif(GA_Multi_Distributed_model_7)
  # units,TVAdstock,OnlineMediaAdstock,AffiliatesAdstock have high vif
  # check the p value for these variables
  # OnlineMediaAdstock is insignificant, hence removing it
  
  #Build Model 8
  GA_Multi_Distributed_model_8<- lm(formula = gmv ~ units + list_price + 
                                      TVAdstock + DigitalAdstock +  
                                      AffiliatesAdstock + isSaleWeek + 
                                      LP_lag_2 + gmv_lag_2 + gmv_lag_3, 
                                    data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_8)
  
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(GA_Multi_Distributed_model_8)
  # units,TVAdstock and AffiliatesAdstock have high vif
  # check the p vlue for these variables
  # TVAdstock is insignificant, hence removing it
  
  #Build Model 9
  GA_Multi_Distributed_model_9<- lm(formula = gmv ~ units + list_price + 
                                      DigitalAdstock +  
                                      AffiliatesAdstock + isSaleWeek + 
                                      LP_lag_2 + gmv_lag_2 + gmv_lag_3, 
                                    data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_9)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  
  vif(GA_Multi_Distributed_model_9)
  # units and AffiliatesAdstock have high vif
  # check the p value for these variables
  # p value is less than 0.05 for these variable which have high vif. Lets remove the insignificant variables
  # DigitalAdstock,isSaleWeek,LP_lag_2,gmv_lag_2,gmv_lag_3 are insignificant, p>0.05. hence removing these variables
  
  #Build Model 10
  GA_Multi_Distributed_model_10<- lm(formula = gmv ~ units + list_price + 
                                      AffiliatesAdstock, 
                                    data = CE_GA_Multi_Distributed_training)
  summary(GA_Multi_Distributed_model_10)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(GA_Multi_Distributed_model_10)
  # All the variables look significant. Hence we can stop the modeling
  
  Final_Multi_Distributed_model_GA <- GA_Multi_Distributed_model_10
  library(DAAG)
  cv_lm <- cv.lm(data = CE_GA_Multi_Distributed_training, form.lm = Final_Multi_Distributed_model_GA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
  ms_GA <- attr(cv_lm,"ms")
  
  # predicting the results in test dataset
  Predict_1 <- predict(Final_Multi_Distributed_model_GA,CE_GA_Multi_Distributed_test)
  CE_GA_Multi_Distributed_test$test_gmv <- Predict_1
  
  ##Now, we need to test the r square between actual and predicted sales. 
  r <- cor(CE_GA_Multi_Distributed_test$gmv,CE_GA_Multi_Distributed_test$test_gmv)
  rsquared <- cor(CE_GA_Multi_Distributed_test$gmv,CE_GA_Multi_Distributed_test$test_gmv)^2
  rsquared
  list(summary(GA_Multi_Distributed_model_10)$r.squared,rsquared)
  
  source("elasticity.R")
  elasticityFun(CE_GA_Multi_Distributed,Final_Multi_Distributed_model_GA,"Gaming Accessory Elasticity Plot")
  return(list(coefficients(Final_Multi_Distributed_model_GA),summary(Final_Multi_Distributed_model_GA)$r.squared,rsquared,ms_GA))
}
