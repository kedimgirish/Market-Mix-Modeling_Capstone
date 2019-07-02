###########Multiplicative Model##########
#######GA#####

GA_Multiplicative_Model <- function(CE_GA_Final)
{

CE_GA_Final_Dataset <- CE_GA_Final

CE_GA_Final_Dataset$LP_lag_1 <- NULL
CE_GA_Final_Dataset$LP_lag_2 <- NULL
CE_GA_Final_Dataset$LP_lag_3 <- NULL

## Replacing 0 value in column with '0.00001' as log(0) is undefined
CE_GA_Final_Dataset[CE_GA_Final_Dataset == 0] <- 0.00001

## Tranforming the negative values
CE_GA_Final_Dataset$priceInf <- 1 + CE_GA_Final_Dataset$priceInf - min(CE_GA_Final_Dataset$priceInf)
View(CE_GA_Final_Dataset)

CE_GA_Final_Dataset <- log(CE_GA_Final_Dataset)
View(CE_GA_Final_Dataset)

#####BUILDING A MODEL ON CE_GA_Final_Dataset#########

## Setting seed to achieve reproducibility
set.seed(1000)

## seperate the Training and test datasets
trainindices = sample(1:nrow(CE_GA_Final_Dataset),0.7*nrow(CE_GA_Final_Dataset))
CE_GA_Multi_training = CE_GA_Final_Dataset[trainindices,]
CE_GA_Multi_test = CE_GA_Final_Dataset[-trainindices,]


# Build model 1 containing all variables
GA_Multi_model_1 <-lm(gmv~.,data=CE_GA_Multi_training)
summary(GA_Multi_model_1)

# Multiple R-squared:1      ,	Adjusted R-squared:1 

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

stepAIC_Multi_model_1 <- stepAIC(GA_Multi_model_1, direction = "both")
stepAIC_Multi_model_1

GA_Multi_model_2 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         discount + TVAdstock + DigitalAdstock + SponsorshipAdstock + 
                         OnlineMediaAdstock + SEMAdstock + sale_frequency + isSaleWeek + 
                         NPS + Moving_Average + priceInf, data = CE_GA_Multi_training)
summary(GA_Multi_model_2)
#Multiple R-squared:1          , Adjusted R-squared: 1
vif(GA_Multi_model_2)

##Removing the variables that have high vif values 
##Removing isSaleWeek  as the vif value is 836.87.
GA_Multi_model_3 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         discount + TVAdstock + DigitalAdstock + SponsorshipAdstock + 
                         OnlineMediaAdstock + SEMAdstock + sale_frequency + 
                         NPS + Moving_Average + priceInf, data = CE_GA_Multi_training)
summary(GA_Multi_model_3)
#Multiple R-squared: 1         , Adjusted R-squared:1 
vif(GA_Multi_model_3)

##Removing the variables that have high vif values 
##Removing TVAdstock,OnlineMediaAdstock  as the vif value is 56.40,90.91 respectively.
GA_Multi_model_4 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         discount +  DigitalAdstock + SponsorshipAdstock + 
                         SEMAdstock + sale_frequency + 
                         NPS + Moving_Average + priceInf, data = CE_GA_Multi_training)
summary(GA_Multi_model_4)
#Multiple R-squared:1          , Adjusted R-squared: 1
vif(GA_Multi_model_4)


##Removing the variables that have high vif values 
##Removing priceInf,SEMAdstock  as the vif value are 18.83,10.38 respectively.
GA_Multi_model_5 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         discount +  DigitalAdstock + SponsorshipAdstock + 
                         sale_frequency + 
                         NPS + Moving_Average, data = CE_GA_Multi_training)
summary(GA_Multi_model_5)
#Multiple R-squared: 1         , Adjusted R-squared: 1
vif(GA_Multi_model_5)

##Removing the variables that have high vif values 
##Removing SponsorshipAdstock  as the vif value is 4.94 .
GA_Multi_model_6 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         discount +  DigitalAdstock + 
                         sale_frequency + 
                         NPS + Moving_Average, data = CE_GA_Multi_training)
summary(GA_Multi_model_6)
#Multiple R-squared: 1        , Adjusted R-squared:1
vif(GA_Multi_model_6)


##Removing the variables that have high p values 
##Removing  NPS, sale_frequency as the p values are 0.408 and 0.237 respectively.
GA_Multi_model_7 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         discount +  DigitalAdstock + 
                         Moving_Average, data = CE_GA_Multi_training)
summary(GA_Multi_model_7)
#Multiple R-squared: 1       , Adjusted R-squared:1
vif(GA_Multi_model_7)

##Removing the variables that have high vif values 
##Removing Discount  as the vif value is 2.24 .
GA_Multi_model_8 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         DigitalAdstock + 
                         Moving_Average, data = CE_GA_Multi_training)
summary(GA_Multi_model_8)
#Multiple R-squared: 1       , Adjusted R-squared:1
vif(GA_Multi_model_8)

##Removing the variables that have high p values 
##Removing Moving_Average  as the p value is 0.106 which is greater then 0.05 .
GA_Multi_model_9 <- lm(formula = gmv ~ units + product_procurement_sla + list_price + 
                         DigitalAdstock , data = CE_GA_Multi_training)
summary(GA_Multi_model_9)
#Multiple R-squared: 1       , Adjusted R-squared:1
vif(GA_Multi_model_9)

##Removing the variables that have high p values 
##Removing DigitalAdstock  as the p value is 0.485 which is greater then 0.05 .
GA_Multi_model_10 <- lm(formula = gmv ~ units + product_procurement_sla + list_price , data = CE_GA_Multi_training)
summary(GA_Multi_model_10)
#Multiple R-squared: 1       , Adjusted R-squared:1
vif(GA_Multi_model_10)

##Removing the variables that have high p values 
##Removing product_procurement_sla as the p value is 0.0654 which is greater then 0.05 .
GA_Multi_model_11 <- lm(formula = gmv ~ units + list_price , data = CE_GA_Multi_training)
summary(GA_Multi_model_11)
#Multiple R-squared: 1       , Adjusted R-squared:1
vif(GA_Multi_model_11)


## As all the p values are below 0.05, we stop modelling here. 
Final_model_GA <- GA_Multi_model_11
library(DAAG)
cv_lm <- cv.lm(data = CE_GA_Multi_training, form.lm = Final_model_GA, m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

GA_ms <- attr(cv_lm,"ms")
# predicting the results in test dataset
Predict_1 <- predict(Final_model_GA,CE_GA_Multi_test)
CE_GA_Multi_test$test_gmv <- Predict_1

##Now, we need to test the r square between actual and predicted sales. 
r <- cor(CE_GA_Multi_test$gmv,CE_GA_Multi_test$test_gmv)
rsquared <- cor(CE_GA_Multi_test$gmv,CE_GA_Multi_test$test_gmv)^2
rsquared##0.9969441

return(list(coefficients(Final_model_GA),summary(Final_model_GA)$r.squared,rsquared,GA_ms))
}