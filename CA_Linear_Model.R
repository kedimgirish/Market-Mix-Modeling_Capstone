CA_Linear_Regression <- function(CE_CA_Final)
{
## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_CA_Final),0.7*nrow(CE_CA_Final))
CE_CA_training = CE_CA_Final[trainindices,]
CE_CA_test = CE_CA_Final[-trainindices,]


# Build model 1 containing all variables
CA_model_1 <-lm(gmv~.,data=CE_CA_training)
summary(CA_model_1)

# Multiple R-squared:  0.998,	Adjusted R-squared:  0.993 

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

stepAIC_model_1 <- stepAIC(CA_model_1, direction = "both")
stepAIC_model_1

CA_model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                   list_price + per_order + TVAdstock + DigitalAdstock + SponsorshipAdstock + 
                   OnlineMediaAdstock + AffiliatesAdstock + SEMAdstock + RadioAdstock + 
                   OtherAdstock + sale_frequency + isSaleWeek + NPS + LP_lag_1 + 
                   LP_lag_2 + Moving_Average + priceInf, data = CE_CA_training)
summary(CA_model_2)
vif(CA_model_2)

# Multiple R-squared:  0.998,	Adjusted R-squared:  0.995
# DigitalAdstock,sale_frequency,units,SponsorshipAdstock,isSaleWeek,product_mrp,OnlineMediaAdstock,NPS,AffiliatesAdstock,LP_lag_1,list_price
# SEMAdstock,LP_lag_2, RadioAdstock, Moving_Average, TVAdstock, OtherAdstock, priceInf
# ## Removing the variables that have a high vif value,
##Removing units,SponsorshipAdstock, isSaleWeek,OnlineMediaAdstock,NPS,AffiliatesAdstock,LP_lag_1,LP_lag_2 due to insignificant values 

CA_model_3 <- lm(formula = gmv ~ week_updated + product_mrp + sla + 
                   list_price + per_order + TVAdstock + DigitalAdstock +  
                   SEMAdstock + RadioAdstock + 
                   OtherAdstock + sale_frequency +  
                   Moving_Average + priceInf, data = CE_CA_training)
summary(CA_model_3)
vif(CA_model_3)
# Multiple R-squared:  0.996,	Adjusted R-squared:  0.993 
# Moving_Average, priceInf, list_price, TVAdstock,DigitalAdstock, SEMAdstock, RadioAdstock, OtherAdstock
# Moving_Average, priceInf, TVAdstock, DigitalAdstock, SEMAdstock, RadioAdstock, OtherAdstock
## Removing SEMAdstock, RadioAdstock ,OtherAdstock due to vif values of 1343.28,1048.75 and 1075.478 respectively 
CA_model_4 <- lm(formula = gmv ~ week_updated + product_mrp + sla + 
                   list_price + per_order +   
                    sale_frequency,  
                    data = CE_CA_training)
summary(CA_model_4)
vif(CA_model_4)
# Multiple R-squared:  0.994,	Adjusted R-squared:  0.992 
# 
## Removing priceInf, list_price due to vif values of 223.422, 131.424 respectively
CA_model_5 <- lm(formula = gmv ~ product_mrp +  
                   list_price + sale_frequency,  
                 data = CE_CA_training)
summary(CA_model_5)
vif(CA_model_5)
# Multiple R-squared:  0.993,	Adjusted R-squared:  0.992 

##Removing the variables that have high vif values 
## Removing units,NPS due to vif values of 26.74, 9.27 respectively
CA_model_6 <- lm(formula = gmv ~ product_mrp +  
                   list_price ,  
                 data = CE_CA_training)
summary(CA_model_6)
vif(CA_model_6)
#Multiple R squared:0.9939       , Adjusted R- squared:0.9907

##As all the p values are less than 0.05 we stop modelling 

Final_linear_model_CA <- CA_model_6
cv_lm <- cv.lm(data = CE_CA_training, form.lm = Final_linear_model_CA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

ms_CA <- attr(cv_lm,"ms")
# predicting the results in test dataset
Predict_1 <- predict(Final_linear_model_CA,CE_CA_test)
CE_CA_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_CA_test$gmv,CE_CA_test$test_gmv)
rsquared <- cor(CE_CA_test$gmv,CE_CA_test$test_gmv)^2
rsquared## 0.9869

return(list(coefficients(Final_linear_model_CA),summary(Final_linear_model_CA)$r.squared,rsquared,ms_CA))
}