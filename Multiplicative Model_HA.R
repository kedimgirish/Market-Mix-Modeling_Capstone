
HA_Multiplicative_Model <- function(CE_HA_Final)
{
CE_HA_Final_Dataset <- CE_HA_Final

CE_HA_Final_Dataset$LP_lag_1 <- NULL
CE_HA_Final_Dataset$LP_lag_2 <- NULL
CE_HA_Final_Dataset$LP_lag_3 <- NULL
  
## Replacing 0 value in column with '0.00001' as log(0) is undefined
CE_HA_Final_Dataset[CE_HA_Final_Dataset == 0] <- 0.00001
  
## Tranforming the negative values
CE_HA_Final_Dataset$priceInf <- 1 + CE_HA_Final_Dataset$priceInf - min(CE_HA_Final_Dataset$priceInf)
View(CE_HA_Final_Dataset)
  
CE_HA_Final_Dataset <- log(CE_HA_Final_Dataset)
View(CE_HA_Final_Dataset)

#####BUILDING A MODEL ON CE_HA_Final_Dataset#########

## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_HA_Final_Dataset),0.7*nrow(CE_HA_Final_Dataset))
CE_HA_Multi_training = CE_HA_Final_Dataset[trainindices,]
CE_HA_Multi_test = CE_HA_Final_Dataset[-trainindices,]


# Build model 1 containing all variables
HA_Multi_model_1 <-lm(gmv~.,data=CE_HA_Multi_training)
summary(HA_Multi_model_1)

# Multiple R-squared:1      ,	Adjusted R-squared:1 

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

stepAIC_Multi_model_1 <- stepAIC(HA_Multi_model_1, direction = "both")
stepAIC_Multi_model_1

HA_Multi_model_2 <- lm(formula = gmv ~ week_updated + units + product_mrp + sla + 
                         list_price + per_order + ContentMediaAdstock + OnlineMediaAdstock + 
                         SEMAdstock + RadioAdstock + NPS + Moving_Average + priceInf, 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_2)
vif(HA_Multi_model_2)
# Multiple R-squared:1      ,	Adjusted R-squared:1 


##Removing the variables that have high vif values 
##Removing units  as the vif value is 1446.83.
HA_Multi_model_3 <- lm(formula = gmv ~ week_updated + product_mrp + sla + 
                         list_price + per_order + ContentMediaAdstock + OnlineMediaAdstock + 
                         SEMAdstock + RadioAdstock + NPS + Moving_Average + priceInf, 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_3)
vif(HA_Multi_model_3)
# Multiple R-squared:0.9994      ,	Adjusted R-squared:0.999

##Removing the variables that have high vif values 
##Removing ContentMediaAdstock and OnlineMediaAdstock  as the vif value is 64.9816 and 77.5896.
HA_Multi_model_4 <- lm(formula = gmv ~ week_updated + product_mrp + sla + 
                         list_price + per_order + 
                         SEMAdstock + RadioAdstock + NPS + Moving_Average + priceInf, 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_4)
vif(HA_Multi_model_4)
# Multiple R-squared:0.9991      ,	Adjusted R-squared: 0.9987


##Removing the variables that have high vif values 
##Removing Moving_Average and List_price  as the vif value is 19.77 and 18.95.
HA_Multi_model_5 <- lm(formula = gmv ~ week_updated + product_mrp + sla + 
                         per_order + SEMAdstock + RadioAdstock + NPS +  priceInf, 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_5)
vif(HA_Multi_model_5)
# Multiple R-squared:0.999      ,	Adjusted R-squared:0.9987

##Removing the variables that have high vif values 
##Removing week_updated  as the vif value is 5.30.
HA_Multi_model_6 <- lm(formula = gmv ~ product_mrp + sla + 
                         per_order + SEMAdstock + RadioAdstock + NPS +  priceInf, 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_6)
vif(HA_Multi_model_6)
# Multiple R-squared: 0.999    ,	Adjusted R-squared:0.9987

##Removing the variables that have high vif values 
##Removing SEMAdstock  as the vif value is 5.52.
HA_Multi_model_7 <- lm(formula = gmv ~ product_mrp + sla + 
                         per_order + RadioAdstock + NPS +  priceInf, 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_7)
vif(HA_Multi_model_7)
# Multiple R-squared:0.9989     ,	Adjusted R-squared:0.9987

##Removing the variables that have high p values of greater than 0.05.
##Removing NPS,per_order as p values are 0.95 and 0.52.
HA_Multi_model_8 <- lm(formula = gmv ~ product_mrp + sla + 
                         RadioAdstock  +  priceInf, 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_8)
vif(HA_Multi_model_8)
# Multiple R-squared: 0.9989    ,	Adjusted R-squared:0.9987

##Removing the variables that have high p values of greater than 0.05.
##Removing priceInf as p value is 0.14.
HA_Multi_model_9 <- lm(formula = gmv ~ product_mrp + sla + 
                         RadioAdstock , 
                       data = CE_HA_Multi_training)
summary(HA_Multi_model_9)
vif(HA_Multi_model_9)
# Multiple R-squared:0.9988    ,	Adjusted R-squared:0.9987

## As all the p values are below 0.05, we stop modelling here. 
Final_Multi_model_HA <- HA_Multi_model_9
library(DAAG)
cv_lm <- cv.lm(data = CE_HA_Multi_training, form.lm = Final_Multi_model_HA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
HA_ms <- attr(cv_lm,"ms")

# predicting the results in test dataset
Predict_1 <- predict(Final_Multi_model_HA,CE_HA_Multi_test)
CE_HA_Multi_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_HA_Multi_test$gmv,CE_HA_Multi_test$test_gmv)
rsquared <- cor(CE_HA_Multi_test$gmv,CE_HA_Multi_test$test_gmv)^2
rsquared## 0.9847

return(list(coefficients(Final_Multi_model_HA),summary(Final_Multi_model_HA)$r.squared,rsquared,HA_ms))
}