library(chillR)
Winters_hours_gaps
hourtemps<-Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]



date_to_date_components<-function(date)
{
  year<-trunc(date/10000)
  month<-trunc((date-year*10000)/100)
  day<-date-(month*100)-year*10000
  return(list(Year=year,Month=month,Day=day,type="Date"))
}


calculate_warm_hours<-function(hourWarm,Warm_temp=25,start_date=NA,end_date=NA)
{
  hourWarm[,"Warm_days"]<-(hourWarm$Temp>Warm_temp)
  start<-date_to_date_components(start_date)
  end<-date_to_date_components(end_date)
  Start_row<-which(hourWarm$Year==start$Year &
                     hourWarm$Month==start$Month &
                     hourWarm$Day==start$Day &
                     hourWarm$Hour==12)
  End_row<-which(hourWarm$Year==end$Year &
                   hourWarm$Month==end$Month &
                   hourWarm$Day==end$Day &
                   hourWarm$Hour==12)
  
  Days<-sum(hourWarm$warm_days[Start_row:End_row])
  return(Days)
  
}

calculate_warm_hours(hourtemps,Warm_temp=25,
                     start_date=20080301,
                     end_date=20080331)

