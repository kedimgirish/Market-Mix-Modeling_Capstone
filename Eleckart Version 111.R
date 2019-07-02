############################################ ElecKart - Capstone Project ############################################

## Install and Load the libraries 
install.packages("dplyr")
install.packages("plyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("readxl")
install.packages("dummies")
install.packages("DAAG")

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(corrplot)
library(MASS)
library(car)
library(dummies)
library(DAAG)

setwd("D:/Learning/UpGrad/Capstone/Ecommerce Capstone/16Jun19")
# Prevent the scientific notation in the dataset(example: order_id, order_item_id, cust_id and pincode)
options(scipen=999)

############################### DATA LOADING and UNDERSTANIDNG ################################

## Load the Data set
ConsumerElectronics <- read.csv("ConsumerElectronics.csv",stringsAsFactors = FALSE)
dim(ConsumerElectronics) #1648824  20
str(ConsumerElectronics)
summary(ConsumerElectronics)


#Media Investment 
media_investment <- read_excel("MediaData.xlsx", sheet = "MediaInvestment")
View(media_investment)
str(media_investment)
summary(media_investment)

#Special Sale Calender
Special_sale_calendar <- read_excel("MediaData.xlsx", sheet = "SpecialSaleCalendar")
View(Special_sale_calendar)
str(Special_sale_calendar)
summary(Special_sale_calendar)

#NPS week data
NPSweekdata <- read_excel("MediaData.xlsx", sheet = "NPSWeekData")
View(NPSweekdata)
str(NPSweekdata)
summary(NPSweekdata)

# AdStock data
AdStock_data <- read_excel("MediaData.xlsx", sheet = "Adstock")
View(AdStock_data)
str(AdStock_data)
summary(AdStock_data)

############################### DATA CLEANING ################################################
# There is some data issues in Year and Month , hence considering order_date and deriving new columns for orderDay, ordermonth, order_year, order_hrs and order_min
ConsumerElectronics$order_date_formatted =  as.POSIXct(ConsumerElectronics$order_date, format = "%Y-%m-%d %H:%M:%S")
View(ConsumerElectronics)

# As per problem statement, we need to consider the data from July 2015 to June 2016. 
# Hence removing the other data from the data set.
ConsumerElectronics_2015_16 <- subset(ConsumerElectronics, order_date_formatted > "2015-07-01 00:00:00" & order_date_formatted < "2016-06-30 23:59:59")
dim(ConsumerElectronics_2015_16) # 1648215 21
View(ConsumerElectronics_2015_16)

# Divide the data in to 3 product categories
# Take care of case sensitivity
ConsumerElectronics_2015_16_sub_cat <- ConsumerElectronics_2015_16[  toupper(ConsumerElectronics_2015_16$product_analytic_sub_category) == "CAMERAACCESSORY" | 
                                                                       toupper(ConsumerElectronics_2015_16$product_analytic_sub_category) == "GAMINGACCESSORY" |
                                                                       toupper(ConsumerElectronics_2015_16$product_analytic_sub_category) == "HOMEAUDIO",]

dim(ConsumerElectronics_2015_16_sub_cat) # 566067     21


#Remove deliverybdays and deliverycdays column as these columns are not useful for analysis.
ConsumerElectronics_2015_16_sub_cat$deliverybdays <- NULL
ConsumerElectronics_2015_16_sub_cat$deliverycdays <- NULL

#Removing the order date column, Year and Month from as a formatted order date, order month and order year columns have been created
ConsumerElectronics_2015_16_sub_cat$order_date <- NULL 
ConsumerElectronics_2015_16_sub_cat$Year <- NULL
ConsumerElectronics_2015_16_sub_cat$Month <- NULL 

# There are some negative values in Order_id, Order_item_id, Cust_id and Pincode columns, lets remove them
ConsumerElectronics_2015_16_sub_cat$order_id <- NULL
ConsumerElectronics_2015_16_sub_cat$order_item_id <- NULL
ConsumerElectronics_2015_16_sub_cat$cust_id<- NULL
ConsumerElectronics_2015_16_sub_cat$pincode <- NULL

#Check for NA values
sum(is.na(ConsumerElectronics_2015_16_sub_cat) == TRUE) #1672


#Remove the rows which has na
ConsumerElectronics_2015_16_sub_cat <- na.omit(ConsumerElectronics_2015_16_sub_cat)
dim(ConsumerElectronics_2015_16_sub_cat) # 564395     12


#Find number of duplicate entries in the dataset
sum(duplicated(ConsumerElectronics_2015_16_sub_cat)) # 37887

#Extract the unique rows (result should be 564395-37355=527040)
ConsumerElectronics_2015_16_sub_cat <- unique(ConsumerElectronics_2015_16_sub_cat)
dim(ConsumerElectronics_2015_16_sub_cat) #526508     12 

# create new column week of year 
ConsumerElectronics_2015_16_sub_cat$week_of_year <- as.numeric(strftime(ConsumerElectronics_2015_16_sub_cat$order_date_formatted,"%V"))
#ConsumerElectronics_2015_16_sub_cat$week_of_year <- week(ConsumerElectronics_2015_16_sub_cat$order_date_formatted)
dim(ConsumerElectronics_2015_16_sub_cat) # 526508     13



# There seems weekly column has some issues. As per problem statement, we are considering data from July 2015 to June 2016 
# Means 1st week starts from July 2015 and last week end at June 2016. Hence if there is data like 1st week in 2016, then we should not consider that.
# From 1st July 2015 to 31st Dec 2015 has 26 weeks 1 day. From 1st July 2015 to 30th June 2016 has 52 weeks 1 day.
# So from Jan 2016, 27th week will start, Hence less than or equal to 26 week count in 2016 year should be modified as 53 week.
# Have to do modify week count for 2015 as well.
#First, deriving the year and month columns from the order date formatted column
ConsumerElectronics_2015_16_sub_cat$order_year <- format(ConsumerElectronics_2015_16_sub_cat$order_date_formatted, "%Y")
ConsumerElectronics_2015_16_sub_cat$order_month <- format(ConsumerElectronics_2015_16_sub_cat$order_date_formatted, "%m")

ConsumerElectronics_2015_16_sub_cat$week_updated <- ifelse(ConsumerElectronics_2015_16_sub_cat$week_of_year <=26 & 
                                                             ConsumerElectronics_2015_16_sub_cat$order_year == 2016,
                                                           ConsumerElectronics_2015_16_sub_cat$week_of_year+53, 
                                                           ConsumerElectronics_2015_16_sub_cat$week_of_year)

# Check the MRP column summary
summary(ConsumerElectronics_2015_16_sub_cat$product_mrp)

# Some products have MRP =0, lets remove those products.
ConsumerElectronics_2015_16_sub_cat <- subset(ConsumerElectronics_2015_16_sub_cat,product_mrp!=0)
dim(ConsumerElectronics_2015_16_sub_cat) #523277     16

# Check the gmv column summary
summary(ConsumerElectronics_2015_16_sub_cat$gmv)

#Some data do not have gmv values, hence removing them
ConsumerElectronics_2015_16_sub_cat <- subset(ConsumerElectronics_2015_16_sub_cat,gmv!=0)
dim(ConsumerElectronics_2015_16_sub_cat)# 523017     16

# gmv= prodcut mrp*no.of units sold. hence gmv value should not be more than mrp*units, so ignoring those values
ConsumerElectronics_2015_16_sub_cat <- subset(ConsumerElectronics_2015_16_sub_cat, (product_mrp*units)>=gmv)
dim(ConsumerElectronics_2015_16_sub_cat) #510196     16

# Mssing values make it 0 in Media investment data
media_investment[is.na(media_investment)] <- 0   

#Treating the outliers present in the sla column 
#Outlier treatment is conducted using the capping method.The data is capped, 
#those observations that lie belowthe 5 percentile mark are replaced with the 5th percentile value and 
#those that lie outside the 95th percentile mark are replaced with the value of the 95th percentile
outlier_treatment <- function(x){
  quantiles <- quantile(x,c(.05,.95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}
# Checking the summary of the sla column 
summary(ConsumerElectronics_2015_16_sub_cat$sla)
boxplot(ConsumerElectronics_2015_16_sub_cat$sla) #There are multiple outliers present in the sla column

ConsumerElectronics_2015_16_sub_cat$sla <- outlier_treatment(ConsumerElectronics_2015_16_sub_cat$sla)

#Checking for the presence of outliers in the product procurement sla column 
boxplot(ConsumerElectronics_2015_16_sub_cat$product_procurement_sla) #There is a presence of multiple outliers 
ConsumerElectronics_2015_16_sub_cat$product_procurement_sla <- outlier_treatment(ConsumerElectronics_2015_16_sub_cat$product_procurement_sla)
dim(ConsumerElectronics_2015_16_sub_cat) #510196     16
View(ConsumerElectronics_2015_16_sub_cat)

## Derive a new dataset with few columns from ConsumerElectronics_2015_16_sub_cat
CE_1 <- ConsumerElectronics_2015_16_sub_cat
CE_1$X...fsn_id <- NULL
CE_1$product_analytic_super_category <- NULL
CE_1$week_of_year <- NULL
dim(CE_1) #510196     13

View(CE_1)

# Divide the 3 sub category product data sets
CE_CameraAccessory <- CE_1[which(CE_1$product_analytic_sub_category == "CameraAccessory"),]
CE_GamingAccessory <- CE_1[which(CE_1$product_analytic_sub_category == "GamingAccessory"),]
CE_HomeAudio <- CE_1[which(CE_1$product_analytic_sub_category == "HomeAudio"),]

Product_KPI <- function(CE_1)
{
  
  # 1. KPI - Derive list price for all the products
  CE_1$list_price <- CE_1$gmv/CE_1$units
  
  #2. KPI - Derive discount for all the products
  CE_1$discount <- round(((CE_1$product_mrp-CE_1$list_price)/CE_1$product_mrp)*100)
  
  #3. KPI - Derive Payment Mode column
  CE_1$Payment_type <- ifelse(CE_1$s1_fact.order_payment_type == "Prepaid",1,0)
  
  # Total Orders placed
  total_order <- aggregate(Payment_type ~week_updated,data =CE_1,FUN=NROW)
  
  #Set the column names to week_updated and total_orders
  total_order <- setNames(total_order,c("week_updated","total_orders"))
  
  #Total Online Order
  online_order <- aggregate(Payment_type ~week_updated,data =CE_1,FUN=sum)
  
  #Set the column names to week_updated and online_orders
  online_order <- setNames(online_order,c("week_updated","online_orders"))
  
  #Merge total order and online order
  TotalOrder_onlineorder <- merge(total_order, online_order, by= c("week_updated"),all.x = TRUE)
  
  #Create new column for Online
  TotalOrder_onlineorder$per_order <- TotalOrder_onlineorder$online_orders/TotalOrder_onlineorder$total_orders
  merged <- TotalOrder_onlineorder[,-c(2,3)]
  CE_1<- merge(CE_1,merged,by=c("week_updated"),all.x=TRUE)
  
}

CE_CA_product  <- Product_KPI(CE_CameraAccessory)
View(CE_CA_product)
dim(CE_CA_product)#214982     18

CE_GA_product<- Product_KPI(CE_GamingAccessory)
View(CE_GA_product)
dim(CE_GA_product) #185081     18

CE_HA_product <- Product_KPI(CE_HomeAudio)
View(CE_HA_product)
dim(CE_HA_product) #110133     18

# Function for weekly aggregation
CE_Weekly_Aggregation <- function(CE_1)
{
  CE_week_Aggregate_1 <- aggregate(cbind(gmv,units,product_mrp)~week_updated,data = CE_1,FUN = sum)
  CE_week_Aggregate_2 <- aggregate(cbind(sla,product_procurement_sla,list_price,discount,per_order)~week_updated, data = CE_1, FUN = mean)
  CE_weekly_data <- merge(CE_week_Aggregate_1,CE_week_Aggregate_2)
  
}

# Calling the function for each sub category and viewing the data
CE_Weekly_Aggregation_CA <- CE_Weekly_Aggregation(CE_CA_product)
View(CE_Weekly_Aggregation_CA) 
dim(CE_Weekly_Aggregation_CA) #52  9

CE_Weekly_Aggregation_GA <- CE_Weekly_Aggregation(CE_GA_product)
View(CE_Weekly_Aggregation_GA)
dim(CE_Weekly_Aggregation_GA)#53 9

CE_Weekly_Aggregation_HA <- CE_Weekly_Aggregation(CE_HA_product)
View(CE_Weekly_Aggregation_HA)
dim(CE_Weekly_Aggregation_HA)#50 9

####################################### KPIs ######################################################################
#5. KPI -Product Premiumness 
#Getting the summary of the mrp of the various products 
summary(CE_1$product_mrp)

##Dividng the product_mrp into 4 columns based on the 1st quartile, median, third quartile into 4 groups so as to create a new column called product premiumness

CE_1$Product_Premiumness[CE_1$product_mrp < quantile(CE_1$product_mrp,0.25) ] <- "Mass Product"
CE_1$Product_Premiumness[CE_1$product_mrp >= quantile(CE_1$product_mrp,0.25) & CE_1$product_mrp < quantile(CE_1$product_mrp,0.50) ] <- "Lower Middle Premium Product"
CE_1$Product_Premiumness[CE_1$product_mrp >= quantile(CE_1$product_mrp,0.50) & CE_1$product_mrp < quantile(CE_1$product_mrp,0.75) ] <- "Upper Middle Premium Product"
CE_1$Product_Premiumness[CE_1$product_mrp >= quantile(CE_1$product_mrp,0.75) & CE_1$product_mrp <= quantile(CE_1$product_mrp,1) ] <- "High Premium Product"

#finding number products for each category
nrow(subset(CE_1, CE_1$Product_Premiumness == "Mass Product")) # 124886
nrow(subset(CE_1, CE_1$Product_Premiumness == "Lower Middle Premium Product")) #127120
nrow(subset(CE_1, CE_1$Product_Premiumness == "Upper Middle Premium Product")) # 128455
nrow(subset(CE_1, CE_1$Product_Premiumness == "High Premium Product")) # 129735

ggplot(CE_1, aes(x = as.factor(Product_Premiumness))) + geom_bar()+labs(x = "Categories", y = "Number of Units" )+ggtitle("Categories vs Number of Units")

#6. KPI -SLA_Timedelay
# Deriving new column for SLA_TimeDelay = sla+product_procurement_sla
CE_1$SLA_TotalTime <- CE_1$sla + CE_1$product_procurement_sla
#View(CE_1)

#Add this new column in merged dataset
CE_1$SLA_TimeDelay = ifelse(CE_1$SLA_TotalTime >= 0 & CE_1$SLA_TotalTime <=5, "NoDelay",ifelse(CE_1$SLA_TotalTime > 5 & CE_1$SLA_TotalTime <= 10, "MinimumDelay","MaximumDelay"))

#finding number products have delay in delivery

nrow(subset(CE_1, CE_1$SLA_TimeDelay == "NoDelay")) #70444
nrow(subset(CE_1, CE_1$SLA_TimeDelay == "MinimumDelay")) #329259
nrow(subset(CE_1, CE_1$SLA_TimeDelay == "MaximumDelay")) #110493

ggplot(CE_1, aes(x = as.factor(SLA_TimeDelay), fill= as.factor(CE_1$Product_Premiumness))) + geom_bar()+labs(x = "Time Delay", y = "Number of Units", fill="Product_Premiumness" )+ggtitle("Time Delay vs Number of Units")

View(CE_1)

#7. KPI - Media Investment

#media_investment$Month <- as.character(media_investment$Month)
#media_investment$Year <- as.character(media_investment$Year)
CE_CA_MI <- merge(CE_Weekly_Aggregation_CA, AdStock_data, by=c("week_updated"), all.x = TRUE)
View(CE_CA_MI)

CE_GA_MI <- merge(CE_Weekly_Aggregation_GA, AdStock_data, by=c("week_updated"), all.x = TRUE)
View(CE_GA_MI)

CE_HA_MI <- merge(CE_Weekly_Aggregation_HA, AdStock_data, by=c("week_updated"), all.x = TRUE)
View(CE_HA_MI)

#8. KPI - Special Sale/Holiday list
#Creating a holiday List column

Special_sale_calendar$Date <- as.Date(Special_sale_calendar$Date)
week_updated <- as.numeric(strftime(Special_sale_calendar$Date,"%V"))
Year <- year(Special_sale_calendar$Date)
Special_sale_calendar_holiday <- data.frame(cbind(Year,week_updated))
Special_sale_calendar_holiday$Date <- Special_sale_calendar$Date
Special_sale_calendar_holiday$sale_frequency <- 1
Special_sale_calendar_holiday<- aggregate(sale_frequency ~Year+week_updated,Special_sale_calendar_holiday,sum )
Special_sale_calendar_holiday$Year <- NULL 
Special_sale_calendar_holiday <- aggregate(sale_frequency~week_updated, Special_sale_calendar_holiday, sum)
View(Special_sale_calendar_holiday)


#As we had converted the week numbers based on our requirement, the week numbers are to be changed based on the code that we followed for the rest of the document 
Special_sale_calendar_holiday$week_updated <- ifelse(Special_sale_calendar_holiday$week_updated <=26,Special_sale_calendar_holiday$week_updated+53,Special_sale_calendar_holiday$week_updated)
View(Special_sale_calendar_holiday)
Special_sale_calendar_holiday$isSaleWeek <- Special_sale_calendar_holiday$sale_frequency
Special_sale_calendar_holiday$isSaleWeek[which(Special_sale_calendar_holiday$sale_frequency>0)] <-1
#write.csv(Special_sale_calendar_holiday,"Special_sale_calendar_holiday.csv", row.names = F)

CE_CA_MI_SSC <- merge(CE_CA_MI, Special_sale_calendar_holiday, by=c("week_updated"), all.x = TRUE)
CE_CA_MI_SSC$sale_frequency[which(is.na(CE_CA_MI_SSC$sale_frequency))] <- 0
CE_CA_MI_SSC$isSaleWeek[which(is.na(CE_CA_MI_SSC$isSaleWeek))] <- 0
View(CE_CA_MI_SSC)

CE_GA_MI_SSC <- merge(CE_GA_MI, Special_sale_calendar_holiday, by=c("week_updated"), all.x = TRUE)
CE_GA_MI_SSC$sale_frequency[which(is.na(CE_GA_MI_SSC$sale_frequency))] <- 0
CE_GA_MI_SSC$isSaleWeek[which(is.na(CE_GA_MI_SSC$isSaleWeek))] <- 0
View(CE_GA_MI_SSC)

CE_HA_MI_SSC <- merge(CE_HA_MI, Special_sale_calendar_holiday, by=c("week_updated"), all.x = TRUE)
CE_HA_MI_SSC$sale_frequency[which(is.na(CE_HA_MI_SSC$sale_frequency))] <- 0
CE_HA_MI_SSC$isSaleWeek[which(is.na(CE_HA_MI_SSC$isSaleWeek))] <- 0
View(CE_HA_MI_SSC)

# 9. KPI - NPS Score
NPSweekdata$Week <- ifelse(NPSweekdata$Week<=26 &NPSweekdata$Year == 2016, NPSweekdata$Week+53,NPSweekdata$Week)

week_updated <- NPSweekdata$Week
NPS <- NPSweekdata$NPS

NPSweekdata_updated <- data.frame(cbind(week_updated,NPS))
View(NPSweekdata_updated)

CE_CA_NPS <- merge(CE_CA_MI_SSC, NPSweekdata_updated, by=c("week_updated"), all.x = TRUE)
View(CE_CA_NPS)


CE_GA_NPS <- merge(CE_GA_MI_SSC, NPSweekdata_updated, by=c("week_updated"), all.x = TRUE)
View(CE_GA_NPS)


CE_HA_NPS <- merge(CE_HA_MI_SSC, NPSweekdata_updated, by=c("week_updated"), all.x = TRUE)
View(CE_HA_NPS)

# 10. KPI - Lag variable

install.packages("DataCombine")
library(DataCombine)

source("LagVariable.R")
CE_GA_NPS_lag <- lagVariable(CE_GA_NPS)
View(CE_GA_NPS_lag)
dim(CE_GA_NPS_lag) #53  24

CE_CA_NPS_lag <- lagVariable(CE_CA_NPS)
View(CE_CA_NPS_lag)
dim(CE_CA_NPS_lag)#52   24

CE_HA_NPS_lag <- lagVariable(CE_HA_NPS)
View(CE_HA_NPS_lag)
dim(CE_HA_NPS_lag)#50   24

install.packages("pracma")
library(pracma)

source("MovingAverage.R")
CE_GA_Moving_Avg <- Moving_Average(CE_GA_NPS_lag)
View(CE_GA_Moving_Avg)
dim(CE_GA_Moving_Avg)#53 26

CE_CA_Moving_Avg <- Moving_Average(CE_CA_NPS_lag)
View(CE_CA_Moving_Avg)
dim(CE_CA_Moving_Avg)#52 26

CE_HA_Moving_Avg <- Moving_Average(CE_HA_NPS_lag)
View(CE_HA_Moving_Avg)
dim(CE_HA_Moving_Avg)#50 26

#Price Inflation
for(loop in 1:nrow(CE_GA_Moving_Avg))
{
  CE_GA_Moving_Avg$priceInf[loop] <- as.integer( (CE_GA_Moving_Avg$list_price[loop] - CE_GA_Moving_Avg$Moving_Average[loop])*100/CE_GA_Moving_Avg$Moving_Average[loop]  )
}

for(loop in 1:nrow(CE_HA_Moving_Avg))
{
  CE_HA_Moving_Avg$priceInf[loop] <- as.integer( (CE_HA_Moving_Avg$list_price[loop] - CE_HA_Moving_Avg$Moving_Average[loop])*100/CE_HA_Moving_Avg$Moving_Average[loop]  )
}

for(loop in 1:nrow(CE_CA_Moving_Avg))
{
  CE_CA_Moving_Avg$priceInf[loop] <- as.integer( (CE_CA_Moving_Avg$list_price[loop] - CE_CA_Moving_Avg$Moving_Average[loop])*100/CE_CA_Moving_Avg$Moving_Average[loop]  )
}


CE_GA_Final <- CE_GA_Moving_Avg
dim(CE_GA_Final)#53   27
CE_HA_Final <- CE_HA_Moving_Avg
dim(CE_HA_Final)#50   27
CE_CA_Final <- CE_CA_Moving_Avg
dim(CE_CA_Final)#52   27


#Removing the unwanted columns 
CE_GA_Final$Row.names <- NULL
CE_HA_Final$Row.names <- NULL 
CE_CA_Final$Row.names <- NULL 


#################################################################### EDA ###############################################################


#Dividing the data into the three datasets depending on the sub categories Camera Accessory, Gaming Accessory and Home Audio to perform EDA 
Camera_Accessory <- ConsumerElectronics_2015_16_sub_cat[ConsumerElectronics_2015_16_sub_cat$product_analytic_sub_category == "CameraAccessory",]
dim(Camera_Accessory) #214982    16

Gaming_Accessory <- ConsumerElectronics_2015_16_sub_cat[ConsumerElectronics_2015_16_sub_cat$product_analytic_sub_category == "GamingAccessory",]
dim(Gaming_Accessory)  #185081   16

Home_Audio <- ConsumerElectronics_2015_16_sub_cat[ConsumerElectronics_2015_16_sub_cat$product_analytic_sub_category == "HomeAudio",]
dim(Home_Audio)  #110133      16

## aggregate by - week and product cateogry
## plot for Week, sales and product category
Camera_Accessory_weekly_gmv <-aggregate(gmv~week_updated, Camera_Accessory, sum, na.rm=TRUE)
Gaming_Accessory_weekly_gmv <-aggregate(gmv~week_updated, Gaming_Accessory, sum, na.rm=TRUE)
Home_Audio_weekly_gmv <-aggregate(gmv~week_updated, Home_Audio, sum, na.rm=TRUE)
View(Camera_Accessory_weekly_gmv)
#merge(Product_Weekly_Aggregate,products,by="week_updated",all.x=TRUE)
Weekly_productwise_GMV<-merge(Camera_Accessory_weekly_gmv,Gaming_Accessory_weekly_gmv,by="week_updated",all.x=TRUE)
Weekly_productwise_GMV<-merge(Weekly_productwise_GMV,Home_Audio_weekly_gmv,by="week_updated",all.x=TRUE)
View(Weekly_productwise_GMV)
Weekly_productwise_GMV$gmv[which(is.na(Weekly_productwise_GMV$gmv))] <- 0

ggplot( Weekly_productwise_GMV, aes(x=as.factor(Weekly_productwise_GMV$week_updated), y = value, color = variable)) + 
  geom_point(aes(y = Weekly_productwise_GMV$gmv.x, col = "Gaming")) + 
  geom_point(aes(y = Weekly_productwise_GMV$gmv.y, col = "HomeAudio"))+
  geom_point(aes(y = Weekly_productwise_GMV$gmv, col = "Camera"))

names(Weekly_productwise_GMV)[names(Weekly_productwise_GMV) == "gmv"] <- "CameraGMV"
names(Weekly_productwise_GMV)[names(Weekly_productwise_GMV) == "gmv.x"] <- "GamingGMV"
names(Weekly_productwise_GMV)[names(Weekly_productwise_GMV) == "gmv.y"] <- "HomeAudioGMV"

ggplot(data = Weekly_productwise_GMV  %>% gather(Variable, GMVValue, -week_updated),  
       aes(x = week_updated, y = GMVValue, fill = Variable)) + 
  geom_bar( stat = 'identity',position = 'dodge')


## Plotting a weekly graph for the gmv of each of the subcategories 

ggplot(data = Gaming_Accessory, aes(x= as.factor(Gaming_Accessory$week_updated)), fill = Gaming_Accessory$gmv) + geom_bar()

ggplot(data = Camera_Accessory, aes(x= as.factor(Camera_Accessory$week_updated), fill = Camera_Accessory$gmv)) + geom_bar()

ggplot(data = Home_Audio, aes(x= as.factor(Home_Audio$week_updated)), fill = Home_Audio$gmv) + geom_bar()

monthly_units_sold <- aggregate(units~order_month, ConsumerElectronics_2015_16_sub_cat, sum, na.rm=TRUE)

#ggplot(monthly_units_sold, aes(x=order_month, y=units)) + geom_bar(stat = "identity", fill="steelblue", width = 15) + 
#  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_minimal() + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + 
#  labs(x="Months",y="Number of Units Sold") + ggtitle("Monthly Units Sold") + 
#  theme(axis.title.x = element_text(colour = "black"),
#        axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),
#        axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),
#        legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


# Number of units
ggplot(ConsumerElectronics_2015_16_sub_cat, aes(units)) + geom_bar()

# No of Units Sold per Month
ggplot(ConsumerElectronics_2015_16_sub_cat, aes(x=factor(order_month),fill=units)) + geom_bar()


weekly_units_sold <-aggregate(units~week_updated, ConsumerElectronics_2015_16_sub_cat, sum, na.rm=TRUE)


ggplot(data = weekly_units_sold, aes(x=as.factor(weekly_units_sold$week_updated),y=units)) + geom_bar(stat = "identity",fill="blue")



### Weekly  nps rating across Product categories
ggplot( CE_CA_Final, aes(x=as.factor(CE_CA_Final$week_updated), y = value, color = variable)) + 
  geom_point(aes(y = CE_CA_Final$NPS, col = "NPS"))  

ggplot( CE_CA_Final, aes(x=as.factor(CE_CA_Final$week_updated), y = value, color = variable)) + 
  geom_point(aes(y = CE_CA_Final$NPS, col = "NPS"))  

ggplot( CE_CA_Final, aes(x=as.factor(CE_CA_Final$week_updated), y = value, color = variable)) + 
  geom_point(aes(y = CE_CA_Final$NPS, col = "NPS"))  


#CameraAccessory - TVAdstock Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$TVAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - Digital Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$DigitalAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - Sponsorship Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$SponsorshipAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - Contentmedia Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$ContentMediaAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - onlinemedia Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$OnlineMediaAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - Affiliates Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$AffiliatesAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - SEM Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$SEMAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - Radio Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$RadioAdstock, gmv)) + geom_point() + geom_smooth()  
#CameraAccessory - Other Vs GMV
ggplot( CE_CA_Final, aes( CE_CA_Final$OtherAdstock, gmv)) + geom_point() + geom_smooth()  



#GamingAccessory - TVAdstock Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$TVAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - Digital Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$DigitalAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - Sponsorship Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$SponsorshipAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - Contentmedia Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$ContentMediaAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - onlinemedia Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$OnlineMediaAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - Affiliates Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$AffiliatesAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - SEM Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$SEMAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - Radio Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$RadioAdstock, gmv)) + geom_point() + geom_smooth()  
#GamingAccessory - Other Vs GMV
ggplot( CE_GA_Final, aes( CE_GA_Final$OtherAdstock, gmv)) + geom_point() + geom_smooth()  


#HomeAudio - TVAdstock Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$TVAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - Digital Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$DigitalAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - Sponsorship Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$SponsorshipAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - Contentmedia Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$ContentMediaAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - onlinemedia Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$OnlineMediaAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - Affiliates Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$AffiliatesAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - SEM Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$SEMAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - Radio Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$RadioAdstock, gmv)) + geom_point() + geom_smooth()  
#HomeAudio - Other Vs GMV
ggplot( CE_HA_Final, aes( CE_HA_Final$OtherAdstock, gmv)) + geom_point() + geom_smooth()  



Model_Table_GA <- data.frame()
Model_Table_CA <- data.frame()
Model_Table_HA <- data.frame()
############################### LINEAR REGRESSION MODEL ###########################
source("GA_Linear_Model.R")
GA_Linear_RValues <- GA_Linear_Regression(CE_GA_Final)
names(GA_Linear_RValues) <- c("coefficients","GA_Train_RSquare","GA_Test_RSquare","Mean Square Error")
GA_Linear_RValues
Model_Table_GA <- GA_Linear_RValues

source("CA_Linear_Model.R")
CA_Linear_RValues <- CA_Linear_Regression(CE_CA_Final)
names(CA_Linear_RValues) <- c("coefficients","CA_Train_RSquare","CA_Test_RSquare","Mean Square Error")
CA_Linear_RValues
Model_Table_CA <- CA_Linear_RValues

source("HA_Linear_Model.R")
HA_Linear_RValues <- HA_Linear_Regression(CE_HA_Final)
names(HA_Linear_RValues) <- c("coefficients","HA_Train_RSquare","HA_Test_RSquare","Mean Square Error")
HA_Linear_RValues
Model_Table_HA <- HA_Linear_RValues

########################### Multiplicate Model ###########################
source("Multiplicative Model_GA.R")
GA_Multiplicative_RValues <- GA_Multiplicative_Model(CE_GA_Final)
names(GA_Multiplicative_RValues) <- c("coefficients","GA_Train_RSquare","GA_Test_RSquare","Mean Square Error")
GA_Multiplicative_RValues
Model_Table_GA <- rbind(Model_Table_GA,GA_Multiplicative_RValues)

source("Multiplicative Model_CA.R")
CA_Multiplicative_RValues <- CA_Multiplicative_Model(CE_CA_Final)
names(CA_Multiplicative_RValues) <- c("coefficients","CA_Train_RSquare","CA_Test_RSquare","Mean Square Error")
CA_Multiplicative_RValues
Model_Table_CA <- rbind(Model_Table_CA,CA_Multiplicative_RValues)

source("Multiplicative Model_HA.R")
HA_Multiplicative_RValues <- HA_Multiplicative_Model(CE_HA_Final)
names(HA_Multiplicative_RValues) <- c("coefficients","HA_Train_RSquare","HA_Test_RSquare","Mean Square Error")
HA_Multiplicative_RValues
Model_Table_HA <- rbind(Model_Table_HA,HA_Multiplicative_RValues)

########################### Koyck  Model ###########################
source("LagVariable_GMV.R")
CE_GA_NPS_Koyck <- lagVariable_Gmv(CE_GA_NPS_lag)
CE_GA_NPS_Koyck$LP_lag_1 <- NULL
CE_GA_NPS_Koyck$LP_lag_2 <- NULL
CE_GA_NPS_Koyck$LP_lag_3 <- NULL
View(CE_GA_NPS_Koyck)
dim(CE_GA_NPS_Koyck) #53  27

CE_CA_NPS_Koyck <- lagVariable_Gmv(CE_CA_NPS_lag)
CE_CA_NPS_Koyck$LP_lag_1 <- NULL
CE_CA_NPS_Koyck$LP_lag_2 <- NULL
CE_CA_NPS_Koyck$LP_lag_3 <- NULL
View(CE_CA_NPS_Koyck)
dim(CE_CA_NPS_Koyck)#52   27

CE_HA_NPS_Koyck <- lagVariable_Gmv(CE_HA_NPS_lag)
CE_HA_NPS_Koyck$LP_lag_1 <- NULL
CE_HA_NPS_Koyck$LP_lag_2 <- NULL
CE_HA_NPS_Koyck$LP_lag_3 <- NULL
View(CE_HA_NPS_Koyck)
dim(CE_HA_NPS_Koyck)#50   24

source("GA_Koyck_Model.R")
GA_Koyck_RValues <- GA_Koyck_Model(CE_GA_NPS_Koyck)
names(GA_Koyck_RValues) <- c("coefficients","GA_Train_RSquare","GA_Test_RSquare","Mean Square Error")
GA_Koyck_RValues
Model_Table_GA <- rbind(Model_Table_GA,GA_Koyck_RValues)

source("CA_Koyck_Model.R")
CA_Koyck_RValues <- CA_Koyck_Model(CE_CA_NPS_Koyck)
names(CA_Koyck_RValues) <- c("coefficients","CA_Train_RSquare","CA_Test_RSquare","Mean Square Error")
CA_Koyck_RValues
Model_Table_CA <- rbind(Model_Table_CA,CA_Koyck_RValues)

source("HA_Koyck_Model.R")
HA_Koyck_RValues <- CA_Koyck_Model(CE_HA_NPS_Koyck)
names(HA_Koyck_RValues) <- c("coefficients","HA_Train_RSquare","HA_Test_RSquare","Mean Square Error")
HA_Koyck_RValues
Model_Table_HA <- rbind(Model_Table_HA,HA_Koyck_RValues)


############################### DISTRIBUTED LAG MODEL ##########################

#Create lag for Dependent variable
source("LagVariable_GMV.R")
CE_GA_NPS_lag_Distributed_Lag <- lagVariable_Gmv(CE_GA_NPS_lag)
View(CE_GA_NPS_lag_Distributed_Lag)
dim(CE_GA_NPS_lag_Distributed_Lag) #53  27

CE_CA_NPS_lag_Distributed_Lag <- lagVariable_Gmv(CE_CA_NPS_lag)
View(CE_CA_NPS_lag_Distributed_Lag)
dim(CE_CA_NPS_lag_Distributed_Lag)#52   27

CE_HA_NPS_lag_Distributed_Lag <- lagVariable_Gmv(CE_HA_NPS_lag)
View(CE_HA_NPS_lag_Distributed_Lag)
dim(CE_HA_NPS_lag_Distributed_Lag)#50   27


source("GA_Distributed_Lag_Regression_Model.R")
GA_Distributed_Lag_RValues <- GA_Distributed_Lag_Regression(CE_GA_NPS_lag_Distributed_Lag)
names(GA_Distributed_Lag_RValues) <- c("coefficients","GA_Train_RSquare","GA_Test_RSquare","Mean Square Error")
GA_Distributed_Lag_RValues
Model_Table_GA <- rbind(Model_Table_GA,GA_Distributed_Lag_RValues)

source("CA_Distributed_Lag_Regression_Model.R")
CA_Distributed_Lag_RValues <- CA_Distributed_Lag_Regression(CE_CA_NPS_lag_Distributed_Lag)
names(CA_Distributed_Lag_RValues) <- c("coefficients","CA_Train_RSquare","CA_Test_RSquare","Mean Square Error")
CA_Distributed_Lag_RValues
Model_Table_CA <- rbind(Model_Table_CA,CA_Distributed_Lag_RValues)

source("HA_Distributed_Lag_Regression_Model.R")
HA_Distributed_Lag_RValues <- HA_Distributed_Lag_Regression(CE_HA_NPS_lag_Distributed_Lag)
names(HA_Distributed_Lag_RValues) <- c("coefficients","HA_Train_RSquare","HA_Test_RSquare","Mean Square Error")
HA_Distributed_Lag_RValues
Model_Table_HA <- rbind(Model_Table_HA,HA_Distributed_Lag_RValues)

################################ Mutiplicate and Distrubuted Model  ################################
source("GA_Multi_distributed_Model.R")
GA_Multi_Distributed_Lag_RValues <- GA_Multi_Distributed_Model(CE_GA_Final)
names(GA_Multi_Distributed_Lag_RValues) <- c("coefficients","GA_Train_RSquare","GA_Test_RSquare","Mean Square Error")
GA_Multi_Distributed_Lag_RValues
Model_Table_GA <- rbind(Model_Table_GA,GA_Multi_Distributed_Lag_RValues)

source("CA_Multi_distributed_Model.R")
CA_Multi_Distributed_Lag_RValues <- CA_Multi_Distributed_Model(CE_CA_Final)
names(CA_Multi_Distributed_Lag_RValues) <- c("coefficients","CA_Train_RSquare","CA_Test_RSquare","Mean Square Error")
CA_Multi_Distributed_Lag_RValues
Model_Table_CA <- rbind(Model_Table_CA,CA_Multi_Distributed_Lag_RValues)

source("HA_Multi_distributed_Model.R")
HA_Multi_Distributed_Lag_RValues <- HA_Multi_Distributed_Model(CE_HA_Final)
names(HA_Multi_Distributed_Lag_RValues) <- c("coefficients","HA_Train_RSquare","HA_Test_RSquare","Mean Square Error")
HA_Multi_Distributed_Lag_RValues
Model_Table_HA <- rbind(Model_Table_HA,HA_Multi_Distributed_Lag_RValues)


write.csv2(Model_Table_GA,"Model_Table_GA.txt",row.names = FALSE)
write.csv2(Model_Table_CA,"Model_Table_CA.txt",row.names = FALSE)
write.csv2(Model_Table_HA,"Model_Table_HA.txt",row.names = FALSE)
