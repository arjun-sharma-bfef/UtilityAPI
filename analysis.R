utilityapiread <- function(){
  require(lubridate)
  intervals_df <- read.csv(file = "src_files/intervals.csv", head=TRUE,sep=",", stringsAsFactors = FALSE)
  if (nrow(intervals_df))
  intervals_df$month <- "NULL" 
  intervals_df$interval_start <- as.POSIXct(intervals_df$interval_start, format ="%m/%d/%Y %H:%M")
  for (i in 1:nrow(intervals_df)){
    intervals_df[i,"month"] <- months(intervals_df[i,"interval_start"])
  }
  
  interval_mod <- intervals_df
  list_remove <- NULL
  for (i in 1:nrow(interval_mod)){
    if (months(intervals_df[i,"interval_start"]) == "May"){
      if (year(intervals_df[i,"interval_start"]) == 2014){
        list_remove <- c(list_remove,i)
      }
    }
    if (months(intervals_df[i,"interval_start"]) == "June"){
      if (year(intervals_df[i,"interval_start"]) == 2015){
        list_remove <- c(list_remove,i)
      }
    }
  }
  interval_mod <- interval_mod[-list_remove,]
  interval_mod <- subset(interval_mod, select=c(interval_start,interval_kWh,month))
  
  first_hour <- hour(intervals_df[nrow(intervals_df),"interval_start"])
  cat("first hour: ", first_hour)
  if (hour(intervals_df[nrow(intervals_df)-4,"interval_start"]) == first_hour+1){
    interval_denom <- 4
  }
  if (hour(intervals_df[nrow(intervals_df)-2,"interval_start"]) == first_hour+1){
    interval_denom <- 2
  }
  if (hour(intervals_df[nrow(intervals_df)-1,"interval_start"]) == first_hour+1){
    interval_denom <- 1
  }
  
  bad_months_list <- ""
  
  day_month_31 <- (31*24*interval_denom)
  day_month_30 <- (30*24*interval_denom)
  day_month_28 <- (28*24*interval_denom)
  day_month_29 <- (29*42*interval_denom)
  if (leap_year(year(intervals_df[i,"interval_start"])+1)){
    Feb_days <- day_month_29
  }
  else{
    Feb_days <- day_month_28
  }
  if (sum(interval_mod$month == "January") == day_month_31){
    print("January has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, January has ', sum(interval_mod$month == "January"))
    #warning('Check the intervals in January')
    cat('\nNumber of hours off by: \n', (day_month_31-sum(interval_mod$month == "January"))/interval_denom)
    bad_months_list <- c(bad_months_list, "January")
  }
  if (sum(interval_mod$month == "February") == Feb_days){
    print("February has the correct amount of lines")
  }
  else{
    cat('Expected 2688 lines, February has ', sum(interval_mod$month == "February"))
    #warning('Check the intervals in February')
    cat('\nNumber of hours off by: \n', (Feb_days-sum(interval_mod$month == "February"))/interval_denom)
    bad_months_list <- c(bad_months_list, "February")
  }
  if (sum(interval_mod$month == "March") == day_month_31){
    print("March has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, March has ', sum(interval_mod$month == "March"))
    #warning('Check the intervals in March')
    cat('\nNumber of hours off by: \n', (day_month_31-sum(interval_mod$month == "March"))/interval_denom)
    bad_months_list <- c(bad_months_list, "March")
  }
  if (sum(interval_mod$month == "April") == day_month_30){
    print("April has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, April has ', sum(interval_mod$month == "April"))
    #warning('Check the intervals in April')
    cat('\nNumber of hours off by: \n', (day_month_30-sum(interval_mod$month == "April"))/interval_denom)
    bad_months_list <- c(bad_months_list, "April")
  }
  if (sum(interval_mod$month == "May") == day_month_31){
    print("May has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, May has ', sum(interval_mod$month == "May"))
    #warning('Check the intervals in May')
    cat('\nNumber of hours off by: \n', (day_month_31-sum(interval_mod$month == "May"))/interval_denom)
    bad_months_list <- c(bad_months_list, "May")
  }
  if (sum(interval_mod$month == "June") == day_month_30){
    print("June has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, June has ', sum(interval_mod$month == "June"))
    #warning('Check the intervals in June')
    cat('\nNumber of hours off by: \n', (day_month_30-sum(interval_mod$month == "June"))/interval_denom)
    bad_months_list <- c(bad_months_list, "June")
  }
  if (sum(interval_mod$month == "July") == day_month_31){
    print("July has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, July has ', sum(interval_mod$month == "July"))
    #warning('Check the intervals in July')
    cat('\nNumber of hours off by: \n', (day_month_31-sum(interval_mod$month == "July"))/interval_denom)
    bad_months_list <- c(bad_months_list, "July")
  }
  if (sum(interval_mod$month == "August") == day_month_31){
    print("August has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, August has ', sum(interval_mod$month == "August"))
    #warning('Check the intervals in August')
    cat('\nNumber of hours off by: \n', (day_month_31-sum(interval_mod$month == "August"))/interval_denom)
    bad_months_list <- c(bad_months_list, "August")
  }
  if (sum(interval_mod$month == "September") == day_month_30){
    print("September has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, September has ', sum(interval_mod$month == "September"))
    #warning('Check the intervals in September')
    cat('\nNumber of hours off by: \n', (day_month_30-sum(interval_mod$month == "September"))/interval_denom)
    bad_months_list <- c(bad_months_list, "September")
  }
  if (sum(interval_mod$month == "October") == day_month_31){
    print("October has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, October has ', sum(interval_mod$month == "October"))
    #warning('Check the intervals in October')
    cat('\nNumber of hours off by: \n', (day_month_31-sum(interval_mod$month == "October"))/interval_denom)
    bad_months_list <- c(bad_months_list, "October")
  }
  if (sum(interval_mod$month == "November") == day_month_30){
    print("November has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, November has ', sum(interval_mod$month == "November"))
    #warning('Check the intervals in November')
    cat('\nNumber of hours off by: \n', (day_month_30-sum(interval_mod$month == "November"))/interval_denom)
    bad_months_list <- c(bad_months_list, "November")
  }
  if (sum(interval_mod$month == "December") == day_month_31){
    print("December has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, December has ', sum(interval_mod$month == "December"))
    #warning('Check the intervals in December')
    cat('\nNumber of hours off by: \n', (day_month_31-sum(interval_mod$month == "December"))/interval_denom)
    bad_months_list <- c(bad_months_list, "December")
  }
  createFixedInterval(interval_mod, interval_denom)
  return(intervals_df)
  return(interval_mod)
  return(final_df)
}

createFixedInterval <- function(interval_mod, interval_denom){
  topInterval <- interval_mod[1,'interval_start']
  bottomInterval <- interval_mod[nrow(interval_mod),'interval_start']
  startDate <- as.POSIXct(bottomInterval, format ="%m/%d/%Y %H:%M")
  endDate <- as.POSIXct(topInterval, format ="%m/%d/%Y %H:%M")
  interval15min <- seq(from=startDate, by="15 mins", length.out = (24*365*interval_denom))
  fixedIntervals_df <- data.frame('interval_start' = interval15min, 'interval_kWh' = rep(0, each=(24*365*interval_denom)))
  View(fixedIntervals_df)
  interval_mod <- subset(interval_mod, select = -(month))
  interval_mod <- interval_mod[!duplicated(interval_mod$interval_start), ]
  interval_mod$interval_start <- as.POSIXct(interval_mod$interval_start, format ="%m/%d/%Y %H:%M")
  View(interval_mod)
  final_df <- merge(interval_mod, fixedIntervals_df, by="interval_start", all.y = TRUE)
  final_df <- final_df[!duplicated(final_df$interval_start), ]
  
  for (i in 1:nrow(final_df)){
    if (is.na(final_df[i,"interval_kWh.x"])){
      final_df[i,"interval_kWh.x"] <- mean(final_df[i+(24*interval_denom),"interval_kWh.x"],final_df[i-(24*interval_denom),"interval_kWh.x"])
    }
  }
  final_df <- subset(final_df, select=c(interval_start,interval_kWh.x))
  View(final_df)
  return(final_df)
}


utilityapiread()



