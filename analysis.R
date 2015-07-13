utilityapiread <- function(){
  intervals_df <- read.csv(file = "src_files/intervals.csv", head=TRUE,sep=",", stringsAsFactors = FALSE)
  intervals_df$month <- "NULL" 
  for (i in 1:nrow(intervals_df)){
    if (grepl("^1/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "January"
    }
    if (grepl("^2/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "February"
    }
    if (grepl("^3/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "March"
    }
    if (grepl("^4/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "April"
    }
    if (grepl("^5/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "May"
    }
    if (grepl("^6/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "June"
    }
    if (grepl("^7/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "July"
    }
    if (grepl("^8/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "August"
    }
    if (grepl("^9/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "September"
    }
    if (grepl("^10/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "October"
    }
    if (grepl("^11/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "November"
    }
    if (grepl("^12/",intervals_df[i,"interval_start"])){
      intervals_df[i,"month"] <- "December"
    }
  }
  
  interval_mod <- intervals_df
  list_remove <- NULL
  for (i in 1:nrow(interval_mod)){
    if (grepl("May",interval_mod[i,"month"])){
      if (grepl("/2014",interval_mod[i,"interval_start"])){
        list_remove <- c(list_remove,i)
      }
    }
    if (grepl("June",interval_mod[i,"month"])){
      if (grepl("/2015",interval_mod[i,"interval_start"])){
        list_remove <- c(list_remove,i)
      }
    }
  }
  interval_mod <- interval_mod[-list_remove,]
  interval_mod <- subset(interval_mod, , select=c(interval_start,interval_kWh,month))
  bad_months_list <- ""
  # Count the number of hours represented
  if (sum(interval_mod$month == "January") == 2976){
    print("January has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, January has ', sum(interval_mod$month == "January"))
    #warning('Check the intervals in January')
    cat('\nNumber of hours off by: \n', (2976-sum(interval_mod$month == "January"))/4)
    bad_months_list <- c(bad_months_list, "January")
  }
  if (sum(interval_mod$month == "February") == 2688){
    print("February has the correct amount of lines")
  }
  else{
    cat('Expected 2688 lines, February has ', sum(interval_mod$month == "February"))
    #warning('Check the intervals in February')
    cat('\nNumber of hours off by: \n', (2688-sum(interval_mod$month == "February"))/4)
    bad_months_list <- c(bad_months_list, "February")
  }
  if (sum(interval_mod$month == "March") == 2976){
    print("March has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, March has ', sum(interval_mod$month == "March"))
    #warning('Check the intervals in March')
    cat('\nNumber of hours off by: \n', (2976-sum(interval_mod$month == "March"))/4)
    bad_months_list <- c(bad_months_list, "March")
  }
  if (sum(interval_mod$month == "April") == 2880){
    print("April has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, April has ', sum(interval_mod$month == "April"))
    #warning('Check the intervals in April')
    cat('\nNumber of hours off by: \n', (2880-sum(interval_mod$month == "April"))/4)
    bad_months_list <- c(bad_months_list, "April")
  }
  if (sum(interval_mod$month == "May") == 2976){
    print("May has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, May has ', sum(interval_mod$month == "May"))
    #warning('Check the intervals in May')
    cat('\nNumber of hours off by: \n', (2976-sum(interval_mod$month == "May"))/4)
    bad_months_list <- c(bad_months_list, "May")
  }
  if (sum(interval_mod$month == "June") == 2880){
    print("June has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, June has ', sum(interval_mod$month == "June"))
    #warning('Check the intervals in June')
    cat('\nNumber of hours off by: \n', (2880-sum(interval_mod$month == "June"))/4)
    bad_months_list <- c(bad_months_list, "June")
  }
  if (sum(interval_mod$month == "July") == 2976){
    print("July has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, July has ', sum(interval_mod$month == "July"))
    #warning('Check the intervals in July')
    cat('\nNumber of hours off by: \n', (2976-sum(interval_mod$month == "July"))/4)
    bad_months_list <- c(bad_months_list, "July")
  }
  if (sum(interval_mod$month == "August") == 2976){
    print("August has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, August has ', sum(interval_mod$month == "August"))
    #warning('Check the intervals in August')
    cat('\nNumber of hours off by: \n', (2976-sum(interval_mod$month == "August"))/4)
    bad_months_list <- c(bad_months_list, "August")
  }
  if (sum(interval_mod$month == "September") == 2880){
    print("September has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, September has ', sum(interval_mod$month == "September"))
    #warning('Check the intervals in September')
    cat('\nNumber of hours off by: \n', (2880-sum(interval_mod$month == "September"))/4)
    bad_months_list <- c(bad_months_list, "September")
  }
  if (sum(interval_mod$month == "October") == 2976){
    print("October has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, October has ', sum(interval_mod$month == "October"))
    #warning('Check the intervals in October')
    cat('\nNumber of hours off by: \n', (2976-sum(interval_mod$month == "October"))/4)
    bad_months_list <- c(bad_months_list, "October")
  }
  if (sum(interval_mod$month == "November") == 2880){
    print("November has the correct amount of lines")
  }
  else{
    cat('Expected 2880 lines, November has ', sum(interval_mod$month == "November"))
    #warning('Check the intervals in November')
    cat('\nNumber of hours off by: \n', (2880-sum(interval_mod$month == "November"))/4)
    bad_months_list <- c(bad_months_list, "November")
  }
  if (sum(interval_mod$month == "December") == 2976){
    print("December has the correct amount of lines")
  }
  else{
    cat('Expected 2976 lines, December has ', sum(interval_mod$month == "December"))
    #warning('Check the intervals in December')
    cat('\nNumber of hours off by: \n', (2976-sum(interval_mod$month == "December"))/4)
    bad_months_list <- c(bad_months_list, "December")
  }
  #march_df <- interval_mod[interval_mod[,2]=="March"]
  write.table(interval_mod, file = "foo.csv",sep = ",")
  find_bad_days(bad_months_list, intervals_df, interval_mod)
  #View(bad_data_df)
  return(intervals_df)
  return(interval_mod)
  return(bad_data_df)
  return(final_df)
}

find_bad_days <- function(bad_months_list, intervals_df, interval_mod){
  #bad_months_list <- c("March","November")
  print("I'm in find_bad_days")
  #cat(bad_months_list)
  bad_data_df <- data.frame("Month" = integer(), "Day" = integer(), "Hour" = integer(), "Present" = integer(),stringsAsFactors = FALSE)
  for (i in bad_months_list){
    if (i == 'January'){
      #Jan_bad_days <- 0
      for (j in 1:31){
        is_in_array <- grepl(sprintf("^1/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          bad_data_row <- c(1,j,0,0)
          bad_data_df <- rbind(bad_data_df,bad_data_row)
          Jan_bad_days <- c(Jan_bad_days,j)
        }
      }
      #return(Jan_bad_days)
    }
    if (i == 'February'){
      #Feb_bad_days <- 0
      for (j in 1:28){
        is_in_array <- grepl(sprintf("^2/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          bad_data_row <- c(2,j,0,0)
          bad_data_df <- rbind(bad_data_df,bad_data_row)
          Feb_bad_days <- c(Feb_bad_days,j)
        }
      }
      #return(Feb_bad_days)
    }
    if (i == 'March'){
      #print("Im in March")
      for (j in 1:31){
        is_in_array <- grepl(sprintf("^3/%d/",j),intervals_df[,"interval_start"])
        if (length(which(is_in_array == 'TRUE')) != 96){
          #bad_data_row <- c(1,j,0,0)
          #bad_data_df <- rbind(bad_data_df,bad_data_row)
          bad_data_df[nrow(bad_data_df)+1, ] <- c(3,j,0,0)
        }
      }
    }
    if (i == 'April'){
      #Apr_bad_days <- 0
      for (j in 1:30){
        is_in_array <- grepl(sprintf("^4/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          Apr_bad_days <- c(Apr_bad_days,j)
        }
      }
      #return(Apr_bad_days)
    }
    if (i == 'May'){
      #May_bad_days <- 0
      for (j in 1:31){
        is_in_array <- grepl(sprintf("^5/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          May_bad_days <- c(May_bad_days,j)
        }
      }
      #return(May_bad_days)
    }
    if (i == 'June'){
      #Jun_bad_days <- 0
      for (j in 1:30){
        is_in_array <- grepl(sprintf("^6/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          Jun_bad_days <- c(Jun_bad_days,j)
        }
      }
      #return(Jun_bad_days)
    }
    if (i == 'July'){
      #Jul_bad_days <- 0
      for (j in 1:31){
        is_in_array <- grepl(sprintf("^7/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          Jul_bad_days <- c(Jul_bad_days,j)
        }
      }
      #return(Jul_bad_days)
    }
    if (i == 'August'){
      #Aug_bad_days <- 0
      for (j in 1:31){
        is_in_array <- grepl(sprintf("^8/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          Aug_bad_days <- c(Aug_bad_days,j)
        }
      }
      #return(Aug_bad_days)
    }
    if (i == 'September'){
      #Sep_bad_days <- 0
      for (j in 1:30){
        is_in_array <- grepl(sprintf("^9/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          Sep_bad_days <- c(Sep_bad_days,j)
        }
      }
      #return(Sep_bad_days)
    }
    if (i == 'October'){
      #Oct_bad_days <- 0
      for (j in 1:31){
        is_in_array <- grepl(sprintf("^10/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          Oct_bad_days <- c(Oct_bad_days,j)
        }
      }
      #return(Oct_bad_days)
    }
    if (i == 'November'){
      #Nov_bad_days <- 0
      for (j in 1:30){
        is_in_array <- grepl(sprintf("^11/%d/",j),intervals_df[,"interval_start"])
        if (length(which(is_in_array == 'TRUE')) != 96){
          #bad_data_row <- c(1,j,0,0)
          #bad_data_df <- rbind(bad_data_df,bad_data_row)
          bad_data_df[nrow(bad_data_df)+1, ] <- c(11,j,0,0)
        }
      }
      #cat("\nnov_bad_days: ",nov_bad_days)
      #return(Nov_bad_days)
    }

    if (i == 'December'){
      #Dec_bad_days <- 0
      for (j in 1:31){
        is_in_array <- grepl(sprintf("^12/j/"),intervals_df[,"interval_start"])
        if ('TRUE' %in% is_in_array){
          count <- count +1
        }
        if (count!=96){
          Dec_bad_days <- c(Dec_bad_days,j)
        }
      }
      #return(Dec_bad_days)
    }
  }
  #print("Hello")
  #cat('March bad days: ', Mar_bad_days)
  #cat('November bad days: ', Nov_bad_days)
  find_bad_hours(bad_data_df, interval_mod)
  return(bad_data_df)
  return(final_df)
}

find_bad_hours <- function(bad_data_df, interval_mod){
  for (row in 1:nrow(bad_data_df)){
    for (hour in 1:23){
      #View(bad_data_df)
      #print(bad_data_df[row,'Month'])
      #print(bad_data_df[row,'Data'])
      #sprintf("^%d/%d/2015 %d:",bad_data_df[row,'Month'],bad_data_df[row,'Day'], hour)
      if (bad_data_df[row,'Month'] == 3){
        year <- 2015
      }
      if (bad_data_df[row,'Month'] == 11){
        year <- 2014
      }
      bool_array <- grepl(sprintf("^%d/%d/%d %d:",bad_data_df[row,'Month'],bad_data_df[row,'Day'], year,hour),interval_mod[,"interval_start"])
      counter_df <- as.data.frame(table(bool_array))
      counter <- counter_df[2,2]
      if (is.na(counter)){
        counter <- 0
      }
      if (counter < 4){
        bad_data_df[row,'Hour'] <- hour
        bad_data_df[row,'Present'] <- 0
      }
      if (counter > 4){
        bad_data_df[row,'Hour'] <- hour
        times_repeated <- counter/4
        bad_data_df[row,'Present'] <- times_repeated
      }
    }
  }
  View(bad_data_df)
  View(interval_mod)
  createFixedInterval(bad_data_df,interval_mod)
  return(bad_data_df)
  return(final_df)
}

createFixedInterval <- function(bad_data_df, interval_mod){
  topInterval <- interval_mod[1,'interval_start']
  bottomInterval <- interval_mod[nrow(interval_mod),'interval_start']
  cat("\ntopInterval: ", topInterval)
  cat("\nbottomInterval: ", bottomInterval)
  startDate <- as.POSIXct(bottomInterval, format ="%m/%d/%Y %H:%M")
  endDate <- as.POSIXct(topInterval, format ="%m/%d/%Y %H:%M")
  interval15min <- seq(from=startDate, by="15 mins", length.out = 35040)
  fixedIntervals_df <- data.frame('interval_start' = interval15min, 'interval_kWh' = rep(0, each=35040))
  View(fixedIntervals_df)
  interval_mod <- subset(interval_mod, select = -(month))
  interval_mod <- interval_mod[!duplicated(interval_mod$interval_start), ]
  interval_mod$interval_start <- as.POSIXct(interval_mod$interval_start, format ="%m/%d/%Y %H:%M")
  View(interval_mod)
  final_df <- merge(interval_mod, fixedIntervals_df, by="interval_start", all.y = TRUE)
  final_df <- final_df[!duplicated(final_df$interval_start), ]
  
  cat("accessinf final_df: ", final_df[14792,"interval_kWh.x"])
  
  for (i in 1:nrow(final_df)){
    if (is.na(final_df[i,"interval_kWh.x"])){
      final_df[i,"interval_kWh.x"] <- mean(final_df[i+672,"interval_kWh.x"],final_df[i-672,"interval_kWh.x"])
    }
  }
  
  #final_df <- final_df[order(as.Date(final_df$interval_start, format="%m/%d/%Y %H:%M")),]
  
  View(final_df)
  return(final_df)
}


utilityapiread()



