HA_Linear_Regression <- function(CE_HA_Final)
{
## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_HA_Final),0.7*nrow(CE_HA_Final))
CE_HA_training = CE_HA_Final[trainindices,]
CE_HA_test = CE_HA_Final[-trainindices,]


# Build model 1 containing all variables
HA_model_1 <-lm(gmv~.,data=CE_HA_training)
summary(HA_model_1)

# Multiple R-squared:0.9996      1,	Adjusted R-squared:0.9984 

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

stepAIC_model_1 <- stepAIC(HA_model_1, direction = "both")
stepAIC_model_1

HA_model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + list_price + 
                   per_order + TVAdstock + DigitalAdstock + SponsorshipAdstock + 
                   ContentMediaAdstock + AffiliatesAdstock + OtherAdstock + 
                   sale_frequency + NPS + LP_lag_1 + LP_lag_2 + Moving_Average, 
                 data = CE_HA_training)
summary(HA_model_2)
vif(HA_model_2)
# Multiple R-squared:0.9995      1,	Adjusted R-squared:0.9991 

##Removing the variables that have high vif values 
## Removing ContentMediaAdstock,DigitalAdstock and product_mrp due to high vif values of 406.42, 219.919 and 238.525 respectively 
HA_model_3 <- lm(formula = gmv ~ week_updated + units + list_price + 
                   per_order + TVAdstock+ SponsorshipAdstock + 
                   AffiliatesAdstock + OtherAdstock + 
                   sale_frequency + NPS + LP_lag_1 + LP_lag_2 + Moving_Average, 
                 data = CE_HA_training)
summary(HA_model_3)
vif(HA_model_3)
# Multiple R-squared:0.999      1,	Adjusted R-squared:0.9984

##Removing the variables that have high vif values 
## Removing list_price, Moving_Average due to high vif values of 172.01 and 120.44  respectively 
HA_model_4 <- lm(formula = gmv ~ week_updated + units + 
                   per_order + TVAdstock+ SponsorshipAdstock + 
                   AffiliatesAdstock + OtherAdstock + 
                   sale_frequency + NPS + LP_lag_1 + LP_lag_2, 
                 data = CE_HA_training)
summary(HA_model_4)
vif(HA_model_4)
# Multiple R-squared:0.9977      1,	Adjusted R-squared:0.9967


##Removing the variables that have high vif values 
## Removing TVAdstock due to high vif value of 5.74  respectively 
HA_model_5 <- lm(formula = gmv ~ week_updated + units + 
                   per_order+ SponsorshipAdstock + 
                   AffiliatesAdstock + OtherAdstock + 
                   sale_frequency + NPS + LP_lag_1 + LP_lag_2, 
                 data = CE_HA_training)
summary(HA_model_5)
vif(HA_model_5)
# Multiple R-squared:0.9977      1,	Adjusted R-squared:0.9968

##Removing the variables that have high vif values 
## Removing LP_lag_1 due to high vif value of 2.23  respectively 
HA_model_6 <- lm(formula = gmv ~ week_updated + units + 
                   per_order+ SponsorshipAdstock + 
                   AffiliatesAdstock + OtherAdstock + 
                   sale_frequency + LP_lag_2, 
                 data = CE_HA_training)
summary(HA_model_6)
vif(HA_model_6)
# Multiple R-squared:0.997      1,	Adjusted R-squared:0.996

##AS all the vif values are below 2, we are now going to remove variables based on p value

## Removing sale_frequency, OtherAdstock, per_order as the p value is greater than 0.05, 0.59,0.72 and 0.37 respectively
HA_model_7 <- lm(formula = gmv ~ week_updated + units + 
                   SponsorshipAdstock + 
                   AffiliatesAdstock + LP_lag_2, 
                 data = CE_HA_training)
summary(HA_model_7)
vif(HA_model_7)
# Multiple R-squared:0.9969      1,	Adjusted R-squared:0.9963

## Removing week_updated as the p value is greater than 0.05, 0.12 respectively
HA_model_8 <- lm(formula = gmv ~ units + 
                   SponsorshipAdstock + 
                   AffiliatesAdstock + LP_lag_2, 
                 data = CE_HA_training)
summary(HA_model_8)
vif(HA_model_8)
# Multiple R-squared:0.9966      1,	Adjusted R-squared:0.9961

## Removing AffiliatesAdstock as the p value is greater than 0.05, 0.1776 respectively
HA_model_9 <- lm(formula = gmv ~ units + 
                   SponsorshipAdstock + LP_lag_2, 
                 data = CE_HA_training)
summary(HA_model_9)
vif(HA_model_9)
# Multiple R-squared:0.9964      1,	Adjusted R-squared:0.996

## As all the p values are below 0.05, we stop modelling here. 
Final_linear_model_HA <- HA_model_9
library(DAAG)

cv_lm <- cv.lm(data = CE_HA_training, form.lm = Final_linear_model_HA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

ms_HA <- attr(cv_lm,"ms")

# predicting the results in test dataset
Predict_1 <- predict(Final_linear_model_HA,CE_HA_test)
CE_HA_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_HA_test$gmv,CE_HA_test$test_gmv)
rsquared <- cor(CE_HA_test$gmv,CE_HA_test$test_gmv)^2
rsquared##0.9839 

return(list(coefficients(Final_linear_model_HA),summary(Final_linear_model_HA)$r.squared,rsquared,ms_HA))
}

