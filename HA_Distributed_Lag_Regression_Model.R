HA_Distributed_Lag_Regression <- function(CE_HA_NPS_lag_Distributed_Lag)
{
## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_HA_NPS_lag_Distributed_Lag),0.7*nrow(CE_HA_NPS_lag_Distributed_Lag))
CE_HA_Distributed_training = CE_HA_NPS_lag_Distributed_Lag[trainindices,]
CE_HA_Distributed_test = CE_HA_NPS_lag_Distributed_Lag[-trainindices,]


# Build model 1 containing all variables
HA_model_1 <-lm(gmv~.,data=CE_HA_Distributed_training)
summary(HA_model_1)
#Multiple R-squared:  1,	Adjusted R-squared:  0.999

stepAIC_model_1 <- stepAIC(HA_model_1, direction = "both")
stepAIC_model_1

Model_2 <- lm(formula = gmv ~ units + product_mrp + sla + product_procurement_sla + 
                list_price + discount + per_order + TVAdstock + SponsorshipAdstock + 
                ContentMediaAdstock + AffiliatesAdstock + SEMAdstock + RadioAdstock + 
                OtherAdstock + sale_frequency + isSaleWeek + NPS + LP_lag_1 + 
                LP_lag_2 + LP_lag_3 + gmv_lag_1 + gmv_lag_2 + gmv_lag_3, 
              data = CE_HA_Distributed_training)

summary(Model_2)
#Multiple R-squared:  1,	Adjusted R-squared:  0.999
sort(vif(Model_2))

Model_3 <- lm(formula = gmv ~ units + sla + 
                TVAdstock + 
                ContentMediaAdstock + AffiliatesAdstock + SEMAdstock + 
                OtherAdstock + 
                LP_lag_2 + gmv_lag_1 + gmv_lag_3, 
              data = CE_HA_Distributed_training)

summary(Model_3)
#Multiple R-squared:  0.998,	Adjusted R-squared:  0.997
sort(vif(Model_3))

Model_4 <- lm(formula = gmv ~ units + sla + 
                TVAdstock + 
                SEMAdstock + 
                OtherAdstock + 
                LP_lag_2, 
              data = CE_HA_Distributed_training)

summary(Model_4)
#Multiple R-squared:  0.996,	Adjusted R-squared:  0.996
sort(vif(Model_4))

Model_5 <- lm(formula = gmv ~ units + 
                TVAdstock + 
                LP_lag_2, 
              data = CE_HA_Distributed_training)

summary(Model_5)
#Multiple R-squared:  0.996,	Adjusted R-squared:  0.995
sort(vif(Model_5))

Model_6 <- lm(formula = gmv ~ units + 
                LP_lag_2, 
              data = CE_HA_Distributed_training)
summary(Model_6)
#Multiple R-squared:  0.994,	Adjusted R-squared:  0.994
sort(vif(Model_6))


## As all the p values are below 0.05, we stop modelling here. 
Final_Distributed_Lag_model_HA <- Model_6
# predicting the results in test dataset
Predict_1 <- predict(Final_Distributed_Lag_model_HA,CE_HA_Distributed_test)
CE_HA_Distributed_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_HA_Distributed_test$gmv,CE_HA_Distributed_test$test_gmv)
rsquared <- cor(CE_HA_Distributed_test$gmv,CE_HA_Distributed_test$test_gmv)^2
rsquared#0.978

crsValildation <- cv.lm(CE_HA_Distributed_training,Final_Distributed_Lag_model_HA,m=10)

msValue <- attr(crsValildation,"ms")
return(list(coefficients(Final_Distributed_Lag_model_HA),summary(Final_Distributed_Lag_model_HA)$r.squared,rsquared,msValue))
}