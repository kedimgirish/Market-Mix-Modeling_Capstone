###########Multiplicative + Distributed Model##########
#######CA#####

CA_Multi_Distributed_Model <- function(CE_CA_Final)
{
 
  CE_CA_lagvariable_Final <-lagVariable_Gmv(CE_CA_Final)
  View(CE_CA_lagvariable_Final)
  CE_CA_lagvariable_Final$Moving_Average <- NULL
  
  ## Replacing 0 value in column with '0.00001' as log(0) is undefined
  CE_CA_lagvariable_Final[CE_CA_lagvariable_Final == 0] <- 0.00001
  
  ## Tranforming the negative values
  CE_CA_lagvariable_Final$priceInf <- 1 + CE_CA_lagvariable_Final$priceInf - min(CE_CA_lagvariable_Final$priceInf)
  #View(CE_CA_lagvariable_Final)
  
  CE_CA_lagvariable_Final$LP_lag_1 <- 1 + CE_CA_lagvariable_Final$LP_lag_1 - min(CE_CA_lagvariable_Final$LP_lag_1)
  CE_CA_lagvariable_Final$LP_lag_2 <- 1 + CE_CA_lagvariable_Final$LP_lag_2 - min(CE_CA_lagvariable_Final$LP_lag_2)
  CE_CA_lagvariable_Final$LP_lag_3 <- 1 + CE_CA_lagvariable_Final$LP_lag_3 - min(CE_CA_lagvariable_Final$LP_lag_3)
  
  CE_CA_lagvariable_Final$gmv_lag_1 <- 1 + CE_CA_lagvariable_Final$gmv_lag_1 - min(CE_CA_lagvariable_Final$gmv_lag_1)
  CE_CA_lagvariable_Final$gmv_lag_2 <- 1 + CE_CA_lagvariable_Final$gmv_lag_2 - min(CE_CA_lagvariable_Final$gmv_lag_2)
  CE_CA_lagvariable_Final$gmv_lag_3 <- 1 + CE_CA_lagvariable_Final$gmv_lag_3 - min(CE_CA_lagvariable_Final$gmv_lag_3)
  #View(CE_CA_lagvariable_Final)
  CE_CA_Multi_Distributed <- log(CE_CA_lagvariable_Final)
  #View(CE_GA_Multi_Distributed)
  
  #####BUILDING A MODEL ON CE_GA_Multi_Distributed dataset#########
  
  ## Setting seed to achieve reproducibility
  set.seed(1000)
  
  ## seperate the Training and test datasets
  trainindices = sample(1:nrow(CE_CA_Multi_Distributed),0.7*nrow(CE_CA_Multi_Distributed))
  CE_CA_Multi_Distributed_training = CE_CA_Multi_Distributed[trainindices,]
  CE_CA_Multi_Distributed_test = CE_CA_Multi_Distributed[-trainindices,]
  
  # Build model 1 containing all variables
  CA_Multi_Distributed_model_1 <-lm(gmv~.,data=CE_CA_Multi_Distributed_training)
  summary(CA_Multi_Distributed_model_1)
  
  # Multiple R-squared:1      ,	Adjusted R-squared:1 
  
  # In stepAIC function, we pass our first model i.e GA_Multi_Distributed_model_1 and 
  # direction is set as both, because in stepwise,  both the forward selection 
  # of variables and backward elimination of variables happen simultaneously.
  
  stepAIC_Multi_Distributed_model_1 <- stepAIC(CA_Multi_Distributed_model_1, direction = "both")
  stepAIC_Multi_Distributed_model_1
  
  # Build Model 2
  CA_Multi_Distributed_model_2 <- lm(formula = gmv ~ units + product_mrp + list_price + discount + 
                                       DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                                       OnlineMediaAdstock + OtherAdstock + isSaleWeek + NPS + LP_lag_1 + 
                                       LP_lag_2 + LP_lag_3 + priceInf + gmv_lag_1 + gmv_lag_2 + 
                                       gmv_lag_3, data = CE_CA_Multi_Distributed_training)
  
  
  summary(CA_Multi_Distributed_model_2)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(CA_Multi_Distributed_model_2)
  # units,OnlineMediaAdstock, priceInf,product_mrp,list_price,discount and ContentMediaAdstock have high vif
  # Check the p value for these varables
  # ContentMediaAdstock is insignificant , hence removing it
  
    # Build Model 3
  CA_Multi_Distributed_model_3 <- lm(formula = gmv ~ units + product_mrp + list_price + discount + 
                                       DigitalAdstock + SponsorshipAdstock +  
                                       OnlineMediaAdstock + OtherAdstock + isSaleWeek + NPS + LP_lag_1 + 
                                       LP_lag_2 + LP_lag_3 + priceInf + gmv_lag_1 + gmv_lag_2 + 
                                       gmv_lag_3, data = CE_CA_Multi_Distributed_training)
  
  
  summary(CA_Multi_Distributed_model_3)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(CA_Multi_Distributed_model_3)
  # units,gmv_lag_1,product_mrp,list_price,discount,LP_lag_1,LP_lag_2,SponsorshipAdstock,LP_lag_3,OnlineMediaAdstock and priceInf have high vif
  # check the p value for these variables
  # discount,SponsorshipAdstock,LP_lag_3 and priceInf are insignificant. hence removing them
  
  # Build Model 4
  CA_Multi_Distributed_model_4 <- lm(formula = gmv ~ units + product_mrp + list_price + 
                                       DigitalAdstock +   
                                       OnlineMediaAdstock + OtherAdstock + isSaleWeek + NPS + LP_lag_1 + 
                                       LP_lag_2 + gmv_lag_1 + gmv_lag_2 + 
                                       gmv_lag_3, data = CE_CA_Multi_Distributed_training)
  
  
  summary(CA_Multi_Distributed_model_4)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(CA_Multi_Distributed_model_4)
  
  #units, product_mrp,LP_lag_2,gmv_lag_1 and list_price have high vif
  # Check the p value for these variables
  # product_mrp and LP_lag_2 are insignificant, hence removing them
  
  # Build Model 5
  CA_Multi_Distributed_model_5 <- lm(formula = gmv ~ units + list_price + 
                                       DigitalAdstock +   
                                       OnlineMediaAdstock + OtherAdstock + isSaleWeek + NPS + LP_lag_1 + 
                                       gmv_lag_1 + gmv_lag_2 + 
                                       gmv_lag_3, data = CE_CA_Multi_Distributed_training)
  
  
  summary(CA_Multi_Distributed_model_5)
  #Multiple R-squared:      1,	Adjusted R-squared:      1
  vif(CA_Multi_Distributed_model_5)
  
  #units,list_price,OnlineMediaAdstock and OtherAdstock have high vif
  #check the p value for these variables
  # OtherAdstock is insignificant, hence removing it
  
  # Build Model 6
  CA_Multi_Distributed_model_6 <- lm(formula = gmv ~ units + list_price + 
                                       DigitalAdstock +   
                                       OnlineMediaAdstock + isSaleWeek + NPS + LP_lag_1 + 
                                       gmv_lag_1 + gmv_lag_2 + 
                                       gmv_lag_3, data = CE_CA_Multi_Distributed_training)
  
  
  summary(CA_Multi_Distributed_model_6)
  # Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(CA_Multi_Distributed_model_6)
  #units, OnlineMediaAdstock and list_price have high vif
  # check the p value for these variables. units, OnlineMediaAdstock and list_price are significant. 
  # Lets remove the insignificant variables
  # DigitalAdstock,isSaleWeek,NPS, LP_lag_1,gmv_lag_1,gmv_lag_2, gmv_lag_3 are insignificant. hence removing them
  
  
  # Build Model 7
  CA_Multi_Distributed_model_7 <- lm(formula = gmv ~ units + list_price + 
                                       OnlineMediaAdstock ,
                                     data = CE_CA_Multi_Distributed_training)
  
  
  summary(CA_Multi_Distributed_model_7)
  #Multiple R-squared:      1,	Adjusted R-squared:      1 
  vif(CA_Multi_Distributed_model_7)
  
  # All the variables look significant. Hence we can stop the modeling
  
  Final_Multi_Distributed_model_CA <- CA_Multi_Distributed_model_7
  
  library(DAAG)
  cv_lm <- cv.lm(data = CE_CA_Multi_Distributed_training, form.lm = Final_Multi_Distributed_model_CA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
  ms_CA <- attr(cv_lm,"ms")
  
  # predicting the results in test dataset
  Predict_1 <- predict(Final_Multi_Distributed_model_CA,CE_CA_Multi_Distributed_test)
  CE_CA_Multi_Distributed_test$test_gmv <- Predict_1
  
  ##Now, we need to test the r square between actual and predicted sales. 
  r <- cor(CE_CA_Multi_Distributed_test$gmv,CE_CA_Multi_Distributed_test$test_gmv)
  rsquared <- cor(CE_CA_Multi_Distributed_test$gmv,CE_CA_Multi_Distributed_test$test_gmv)^2
  rsquared
  
  source("elasticity.R")
  elasticityFun(CE_CA_Multi_Distributed,Final_Multi_Distributed_model_CA,"Camera Accessory Elasticity Plot")
  
  return(list(coefficients(Final_Multi_Distributed_model_CA),summary(Final_Multi_Distributed_model_CA)$r.squared,rsquared,ms_CA))
}
