GA_Distributed_Lag_Regression <- function(CE_GA_NPS_lag_Distributed_Lag)
{

## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_GA_NPS_lag_Distributed_Lag),0.7*nrow(CE_GA_NPS_lag_Distributed_Lag))
CE_GA_training = CE_GA_NPS_lag_Distributed_Lag[trainindices,]
CE_GA_test = CE_GA_NPS_lag_Distributed_Lag[-trainindices,]


# Build model 1 containing all variables
GA_model_1 <-lm(gmv~.,data=CE_GA_training)
summary(GA_model_1)
#Multiple R-squared:  0.999,	Adjusted R-squared:  0.995 


stepAIC_model_1 <- stepAIC(GA_model_1, direction = "both")
stepAIC_model_1


Model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                product_procurement_sla + list_price + discount + per_order + 
                TVAdstock + DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                OnlineMediaAdstock + AffiliatesAdstock + SEMAdstock + RadioAdstock + 
                OtherAdstock + isSaleWeek + LP_lag_1 + LP_lag_2 + LP_lag_3 + 
                gmv_lag_1 + gmv_lag_2 + gmv_lag_3, data = CE_GA_training)

summary(Model_2)
#Multiple R-squared:  0.999,	Adjusted R-squared:  0.996
sort(vif(Model_2))

Model_3 <- lm(formula = gmv ~ units + product_mrp + sla + 
                SponsorshipAdstock  + 
                RadioAdstock + 
                OtherAdstock, data = CE_GA_training)

summary(Model_3)
#Multiple R-squared:  0.926,	Adjusted R-squared:  0.912
vif(Model_3)

Model_4 <- lm(formula = gmv ~ units + product_mrp
                , data = CE_GA_training)

summary(Model_4)
#Multiple R-squared:  0.914,	Adjusted R-squared:  0.909
vif(Model_4)


Model_5 <- lm(formula = gmv ~ units
              , data = CE_GA_training)
summary(Model_5)
#Multiple R-squared:  0.898,	Adjusted R-squared:  0.895
vif(Model_5)


## As all the p values are below 0.05, we stop modelling here. 
Final_Distributed_Lag_model_GA <- Model_5
# predicting the results in test dataset
Predict_1 <- predict(Final_Distributed_Lag_model_GA,CE_GA_test)
CE_GA_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_GA_test$gmv,CE_GA_test$test_gmv)
rsquared <- cor(CE_GA_test$gmv,CE_GA_test$test_gmv)^2
rsquared##0.94

crsValildation <- cv.lm(CE_GA_training,Final_Distributed_Lag_model_GA,m=4)
msValue <- attr(crsValildation,"ms")#263106860518

return(list(coefficients(Final_Distributed_Lag_model_GA),summary(Final_Distributed_Lag_model_GA)$r.squared,rsquared,msValue))
}