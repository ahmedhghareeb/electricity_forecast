library(forecast)
############################################################################
# Read in and plot the National Grid Data
#http://www.nationalgrid.com/uk/electricity/data/demand+data/
############################################################################
demand_data <- read.csv(file="DemandData_Historic.csv")

t <- (nrow(demand_data)-50*7*2):nrow(demand_data)
labels <- paste(format(as.POSIXct(demand_data$settlement_date[t],format="%d/%m/%Y %H:%M"),"%A"),"-",demand_data$settlement_period,sep="")
par(las=2)
par(mar=c(8,4,3,1))
plot(demand_data$i014_TGSD[t],main="Total Gross System Demand",ylab="Demand",axes=FALSE,xlab="")
axis(2)
axis(1,at=seq(1,50*7*2,length=50),labels[seq(1,50*7*2,length=50)])

############################################################################
# Derive some additional variables for modelling from the demand data
############################################################################

day <- format(as.POSIXct(demand_data$settlement_date,format="%d/%m/%Y %H:%M"),"%A")
date <- format(as.POSIXct(demand_data$settlement_date,format="%d/%m/%Y %H:%M"),"%Y-%m-%d")
demand_data <- cbind(demand_data,date,day)

############################################################################
# Read in some weather data (from London area)
# you will need to obtain your own weather data - there are many site that make it available
# but get in touch if you need help with this.
############################################################################

weather_data <- read.csv(file="London_weather_01.csv",header=TRUE)
weather_data <- weather_data[,-which(is.element(names(weather_data),c("weather_id","location_id","created")))]
temp         <- weather_data$temperature
weather_data <- cbind(weather_data,temp)
head(weather_data)

############################################################################
# Merge the two datasets according to common dates
############################################################################

combined_data <- merge(demand_data,weather_data,by.x="date",by.y="date",all.x=TRUE)
head(combined_data);dim(combined_data)
# taking out correct time range for which we have both sets of data:
rows <- which(as.Date(combined_data$date)>=as.Date("2012-07-02")&as.Date(combined_data$date)<as.Date("2014-06-29"))
cdata <- combined_data[rows,]

############################################################################
# Do some cleaning up - remove daylight savings time records etc
############################################################################

cdata$temp<-ifelse(is.na(cdata$temp),52.142,cdata$temp)
cdata <- cdata[-unique(c(which(is.na(cdata$i014_TGSD))),which(is.na(cdata$day))),]
cdata <- cdata[-unique(c(which((cdata$settlement_period>48)))),]
cdata <- cdata[order(cdata$date,cdata$settlement_period),]
data_for_excel_test <- cdata[((nrow(cdata)-47):(nrow(cdata))),]
cdata <- cdata[-((nrow(cdata)-47):(nrow(cdata))),]
############################################################################
# Create a new dataset containing the variables to be used in the model
############################################################################
df       <- data.frame(day = factor(cdata$day,levels=c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")), temp=as.numeric(cdata$temp),hh=factor(cdata$settlement_period,levels=seq(1,48,by=1)))
model_df <- cbind(Day=model.matrix(~as.factor(df$day)), HH=model.matrix(~as.factor(df$hh)),Temp=df$temp)
model_df <- model_df[,-c(1,8)]#grep("(Intercept)",names(model_df))

############################################################################
# The response must be a time series object
############################################################################
demand <- ts(cdata$i014_TGSD, frequency=48)
############################################################################
# Fit the model and plot a forecast
# for speed & time purposes, only the last X% of the data is used
############################################################################
range     <- seq(round(nrow(model_df)-nrow(model_df)*0.3,0),nrow(model_df))
demand    <- demand[range]
model_df  <- model_df[range,]
arima_fit <- Arima(demand,order=c(1,0,0),xreg=model_df,seasonal=c(0,0,0))
fc        <- forecast(arima_fit,xreg=tail(model_df,10))
plot(fc)

############################################################################
# Create a simple prediction function to help with deploying the model
############################################################################
arima_predict_fn <- function(model_object,new_data){  
  df       <- data.frame(day = factor(new_data$day,levels=c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")), temp=as.numeric(new_data$temp),hh=factor(new_data$settlement_period,levels=seq(1,48,by=1)));
  model_df <- cbind(Day=model.matrix(~as.factor(df$day)), HH=model.matrix(~as.factor(df$hh)),Temp=df$temp);
  model_df <- model_df[,-c(1,8)];
  if (is.null(dim(model_df))) {model_df <- as.matrix(t(model_df))};
  fc <- forecast(model_object,xreg=model_df)$mean;
  return(fc);
}
### give it a quick test
res <- arima_predict_fn(arima_fit, tail(cdata[,c("settlement_period","day","temp")],5))

############################################################################
# Deploy the model ,the special predict function needs to be used
############################################################################
############################################################################
# duke analytics deployment details
############################################################################
library(ddeploy)
## please register here http://www.dukeanalytics.com/products/account/registration.php
## to get your own API key details and fill them in here
duke_config <- 
  list(
    user_name="try_it",
    api_key="db1542b66f16aba5768d8a19c27dec4facf9168a",
    endpoint="/api/v1.0"
  )
## deploy the model
duke_deploy(duke_config,arima_fit,predict_udf=arima_predict_fn)
## make forecasts with the deployed model
fc_deployed <- duke_predict(duke_config,"arima_fit",data_for_excel_test)
plot(fc_deployed)


write.csv(file="for_excel_test.csv",data_for_excel_test,row.names=FALSE)

