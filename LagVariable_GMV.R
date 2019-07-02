lagVariable_Gmv <- function(dataframe)
{
  # Lag variable for Gaming accessories
  #List of list price by 1,2,3 weeks (Date values are ordered)
  #Previous List price
  
  data_LP_GA <- slide(dataframe, Var = "gmv",slideBy = -1)
  data_LP_GA <- slide(data_LP_GA, Var = "gmv", slideBy = -2)
  data_LP_GA <- slide(data_LP_GA, Var = "gmv",slideBy = -3)
  
  CE_2_GA_lag_var <- data_LP_GA
  
  # Incremental lag of list price and discounts
  CE_2_GA_lag_var$gmv_lag_1 <- (CE_2_GA_lag_var$gmv - CE_2_GA_lag_var$`gmv-1`)/CE_2_GA_lag_var$`gmv-1`
  CE_2_GA_lag_var$gmv_lag_2 <- (CE_2_GA_lag_var$gmv - CE_2_GA_lag_var$`gmv-2`)/CE_2_GA_lag_var$`gmv-2`
  CE_2_GA_lag_var$gmv_lag_3 <- (CE_2_GA_lag_var$gmv - CE_2_GA_lag_var$`gmv-3`)/CE_2_GA_lag_var$`gmv-3`
  
  CE_2_GA_lag_var$gmv_lag_1 <- ifelse(is.na(CE_2_GA_lag_var$gmv_lag_1),0,CE_2_GA_lag_var$gmv_lag_1)
  CE_2_GA_lag_var$gmv_lag_2 <- ifelse(is.na(CE_2_GA_lag_var$gmv_lag_2),0,CE_2_GA_lag_var$gmv_lag_2)
  CE_2_GA_lag_var$gmv_lag_3 <- ifelse(is.na(CE_2_GA_lag_var$gmv_lag_3),0,CE_2_GA_lag_var$gmv_lag_3)
  
  
  CE_2_GA_lag_var$`gmv-1` <- NULL
  CE_2_GA_lag_var$`gmv-2` <- NULL
  CE_2_GA_lag_var$`gmv-3` <- NULL
  
  
  return(CE_2_GA_lag_var)
}
