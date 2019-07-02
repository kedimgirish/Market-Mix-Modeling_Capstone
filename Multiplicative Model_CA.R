###########Multiplicative Model##########
#######CA#####

CA_Multiplicative_Model <- function(CE_CA_Final)
{

CE_CA_Final_Dataset <- CE_CA_Final

CE_CA_Final_Dataset$LP_lag_1 <- NULL
CE_CA_Final_Dataset$LP_lag_2 <- NULL
CE_CA_Final_Dataset$LP_lag_3 <- NULL

## Replacing 0 value in column with '0.00001' as log(0) is undefined
CE_CA_Final_Dataset[CE_CA_Final_Dataset == 0] <- 0.00001

## Tranforming the negative values
CE_CA_Final_Dataset$priceInf <- 1 + CE_CA_Final_Dataset$priceInf - min(CE_CA_Final_Dataset$priceInf)
View(CE_CA_Final_Dataset)

CE_CA_Final_Dataset <- log(CE_CA_Final_Dataset)
View(CE_CA_Final_Dataset)

#####BUILDING A MODEL ON CE_GA_Final_Dataset#########

## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_CA_Final_Dataset),0.7*nrow(CE_CA_Final_Dataset))
CE_CA_Multi_training = CE_CA_Final_Dataset[trainindices,]
CE_CA_Multi_test = CE_CA_Final_Dataset[-trainindices,]


# Build model 1 containing all variables
CA_Multi_model_1 <-lm(gmv~.,data=CE_CA_Multi_training)
summary(CA_Multi_model_1)

# Multiple R-squared:1      ,	Adjusted R-squared:1 

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

stepAIC_Multi_model_1 <- stepAIC(CA_Multi_model_1, direction = "both")
stepAIC_Multi_model_1

CA_Multi_model_2 <- lm(formula = gmv ~ units + list_price + SponsorshipAdstock + 
                         OnlineMediaAdstock + AffiliatesAdstock + RadioAdstock + OtherAdstock + 
                         sale_frequency + Moving_Average + priceInf, data = CE_CA_Multi_training)
summary(CA_Multi_model_2)
# Multiple R-squared:1      ,	Adjusted R-squared:1
vif(CA_Multi_model_2)
## Removing the variables that have high vif values 
## Removing RadioAdstock as the vif value is 2838.35

CA_Multi_model_3 <- lm(formula = gmv ~ units + list_price + SponsorshipAdstock + 
                         OnlineMediaAdstock + AffiliatesAdstock +OtherAdstock + 
                         sale_frequency + Moving_Average + priceInf, data = CE_CA_Multi_training)
summary(CA_Multi_model_3)
# Multiple R-squared:1      ,	Adjusted R-squared:1
vif(CA_Multi_model_3)

## Removing the variables that have high vif values 
## Removing OnlineMediaAdstock and AffiliatesAdstock as the vif values are 1244.16 and 1198.36

CA_Multi_model_4 <- lm(formula = gmv ~ units + list_price + SponsorshipAdstock + OtherAdstock + 
                         sale_frequency + Moving_Average + priceInf, data = CE_CA_Multi_training)
summary(CA_Multi_model_4)
# Multiple R-squared:1      ,	Adjusted R-squared:1
vif(CA_Multi_model_4)

## Removing the variables that have high vif values 
## Removing list_price as the vif value is 16.85

CA_Multi_model_5 <- lm(formula = gmv ~ units + SponsorshipAdstock + OtherAdstock + 
                         sale_frequency + Moving_Average + priceInf, data = CE_CA_Multi_training)
summary(CA_Multi_model_5)
# Multiple R-squared:0.9989      ,	Adjusted R-squared:0.9987
vif(CA_Multi_model_5)

## Removing the variables that have high vif values 
## Removing priceInf as the vif value is 2.43

CA_Multi_model_6 <- lm(formula = gmv ~ units + SponsorshipAdstock + OtherAdstock + 
                         sale_frequency + Moving_Average , data = CE_CA_Multi_training)
summary(CA_Multi_model_6)
# Multiple R-squared:0.9969      ,	Adjusted R-squared:0.9964
vif(CA_Multi_model_6)

##As all the vif values are low, we now remove the variables based on p values 
## Removing sale_frequency and SponsorshipAdstock as the p value is 0.347 and 0.161

CA_Multi_model_7 <- lm(formula = gmv ~ units + OtherAdstock + 
                         Moving_Average , data = CE_CA_Multi_training)
summary(CA_Multi_model_7)
# Multiple R-squared: 0.9966     ,	Adjusted R-squared: 0.9963
vif(CA_Multi_model_7)
## As all the variables are significant we stop modelling here. 

## As all the p values are below 0.05, we stop modelling here. 
Final_Multi_model_CA <- CA_Multi_model_7

library(DAAG)
cv_lm <- cv.lm(data = CE_CA_Multi_training, form.lm = Final_Multi_model_CA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

CA_ms <- attr(cv_lm,"ms")

# predicting the results in test dataset
Predict_1 <- predict(Final_Multi_model_CA,CE_CA_Multi_test)
CE_CA_Multi_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_CA_Multi_test$gmv,CE_CA_Multi_test$test_gmv)
rsquared <- cor(CE_CA_Multi_test$gmv,CE_CA_Multi_test$test_gmv)^2
rsquared## 0.9922

return(list(coefficients(Final_Multi_model_CA),summary(Final_Multi_model_CA)$r.squared,rsquared,CA_ms))
}