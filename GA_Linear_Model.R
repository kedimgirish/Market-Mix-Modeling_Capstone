GA_Linear_Regression <- function(CE_GA_Final)
{
## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_GA_Final),0.7*nrow(CE_GA_Final))
CE_GA_training = CE_GA_Final[trainindices,]
CE_GA_test = CE_GA_Final[-trainindices,]


# Build model 1 containing all variables
GA_model_1 <-lm(gmv~.,data=CE_GA_training)
summary(GA_model_1)

# Multiple R-squared:0.9939      1,	Adjusted R-squared:0.98 

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

stepAIC_model_1 <- stepAIC(GA_model_1, direction = "both")
stepAIC_model_1

GA_model_2 <-lm(formula = gmv ~ units + sla + list_price + discount + TVAdstock + 
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  AffiliatesAdstock + SEMAdstock + RadioAdstock + OtherAdstock + 
                  NPS + LP_lag_2 + Moving_Average + priceInf, data = CE_GA_training)
summary(GA_model_2)
vif(GA_model_2)
##Multiple R squared:0.993      Adjusted R squared:0.9873

##Removing the variables that have high vif values 
## Removing SEMAdstock due to high vif value of 991.05 respectively

GA_model_3 <-lm(formula = gmv ~ units + sla + list_price + discount + TVAdstock + 
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  AffiliatesAdstock + RadioAdstock + OtherAdstock + 
                  NPS + LP_lag_2 + Moving_Average + priceInf, data = CE_GA_training)
summary(GA_model_3)
vif(GA_model_3)
##Multiple R squared:0.9913      Adjusted R squared:0.985

##Removing the variables that have high vif values 
## Removing priceInf,list_price due to high vif value of 111.25 and 126.46  respectively

GA_model_4 <-lm(formula = gmv ~ units + sla + discount + TVAdstock + 
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  AffiliatesAdstock + RadioAdstock + OtherAdstock + 
                  NPS + LP_lag_2 + Moving_Average, data = CE_GA_training)
summary(GA_model_4)
vif(GA_model_4)
##Multiple R squared:0.9868      Adjusted R squared:0.9793

##Removing the variables that have high vif values 
## Removing RadioAdstock due to high vif value of 104.48 respectively

GA_model_5 <-lm(formula = gmv ~ units + sla + discount + TVAdstock + 
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  AffiliatesAdstock + OtherAdstock + 
                  NPS + LP_lag_2 + Moving_Average, data = CE_GA_training)
summary(GA_model_5)
vif(GA_model_5)
##Multiple R squared:0.9771      Adjusted R squared:0.9656

##Removing the variables that have high p values 
## Removing LP_lag_2,discount,sla,NPS due to high p value of 0.40,0.62,0.54 and 0.26, which are greater than 0.05 respectively

GA_model_6 <-lm(formula = gmv ~ units + TVAdstock + 
                  DigitalAdstock + SponsorshipAdstock + ContentMediaAdstock + 
                  AffiliatesAdstock + OtherAdstock + 
                 Moving_Average, data = CE_GA_training)
summary(GA_model_6)
vif(GA_model_6)
##Multiple R squared:0.9773      Adjusted R squared:0.9657

##Removing the variables that have high vif values 
## Removing AffiliatesAdstock, DigitalAdstock due to high vif values of 20.54,41.01  respectively

GA_model_7 <-lm(formula = gmv ~ units + TVAdstock + 
                  SponsorshipAdstock + ContentMediaAdstock + OtherAdstock + 
                  Moving_Average, data = CE_GA_training)
summary(GA_model_7)
vif(GA_model_7)
##Multiple R squared:0.9651      Adjusted R squared:0.9581

##Removing the variables that have high p values 
## Removing TVAdstock, OthersAdstock due to high p values of 0.144, 0.064  respectively

GA_model_8 <-lm(formula = gmv ~ units +  
                  SponsorshipAdstock + ContentMediaAdstock +  
                  Moving_Average, data = CE_GA_training)
summary(GA_model_8)
vif(GA_model_8)
##Multiple R squared:0.9607      Adjusted R squared:0.9558

##All the p values are below 0.05 and the vif values are below 3 so we stop modelling. 

Final_linear_model_GA <- GA_model_8
library(DAAG)
cv_lm <- cv.lm(data = CE_GA_training, form.lm = Final_linear_model_GA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

ms_GA <- attr(cv_lm,"ms")

# predicting the results in test dataset
Predict_1 <- predict(Final_linear_model_GA,CE_GA_test)
CE_GA_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_GA_test$gmv,CE_GA_test$test_gmv)
rsquared <- cor(CE_GA_test$gmv,CE_GA_test$test_gmv)^2
rsquared##0.9380

return(list(coefficients(Final_linear_model_GA),summary(Final_linear_model_GA)$r.squared,rsquared,ms_GA))
}