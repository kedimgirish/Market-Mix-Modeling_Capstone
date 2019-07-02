lagVariable <- function(dataframe)
{
# Lag variable for Gaming accessories
#List of list price by 1,2,3 weeks (Date values are ordered)
#Previous List price

data_LP_GA <- slide(dataframe, Var = "list_price",slideBy = -1)
data_LP_GA <- slide(data_LP_GA, Var = "list_price", slideBy = -2)
data_LP_GA <- slide(data_LP_GA, Var = "list_price",slideBy = -3)

CE_2_GA_lag_var <- data_LP_GA

# Incremental lag of list price and discounts
CE_2_GA_lag_var$LP_lag_1 <- (CE_2_GA_lag_var$list_price - CE_2_GA_lag_var$`list_price-1`)/CE_2_GA_lag_var$`list_price-1`
CE_2_GA_lag_var$LP_lag_2 <- (CE_2_GA_lag_var$list_price - CE_2_GA_lag_var$`list_price-2`)/CE_2_GA_lag_var$`list_price-2`
CE_2_GA_lag_var$LP_lag_3 <- (CE_2_GA_lag_var$list_price - CE_2_GA_lag_var$`list_price-3`)/CE_2_GA_lag_var$`list_price-3`

CE_2_GA_lag_var$LP_lag_1 <- ifelse(is.na(CE_2_GA_lag_var$LP_lag_1),0,CE_2_GA_lag_var$LP_lag_1)
CE_2_GA_lag_var$LP_lag_2 <- ifelse(is.na(CE_2_GA_lag_var$LP_lag_2),0,CE_2_GA_lag_var$LP_lag_2)
CE_2_GA_lag_var$LP_lag_3 <- ifelse(is.na(CE_2_GA_lag_var$LP_lag_3),0,CE_2_GA_lag_var$LP_lag_3)


CE_2_GA_lag_var$`list_price-1` <- NULL
CE_2_GA_lag_var$`list_price-2` <- NULL
CE_2_GA_lag_var$`list_price-3` <- NULL


return(CE_2_GA_lag_var)
}
