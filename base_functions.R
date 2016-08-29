# Splitting S&P Chart into Functions

## Pull Down S&P data from Yahoo Finance with user specified date range.
## Currently must pass it a Date Object
## Returns Data Frame of SP Data
get_s_p <- function(start_date, end_date){
  end_day <- day(end_date)
  end_month<- month(end_date)
  end_year = year(end_date)
  
  start_day <- day(start_date)
  start_month <- month(start_date)
  start_year <- year(start_date)
  
  s_p_url <- paste0("http://chart.finance.yahoo.com/table.csv?s=^GSPC&a=",start_month-1,"&b=", start_day, "&c=", start_year,"&d=", end_month-1, "&e=", end_day, "&f=", end_year, "&g=d&ignore=.csv")
  
  #print(s_p_url) }

  s_p_data <- getURL(s_p_url)
  s_p_data <- read_csv(s_p_data)
  print(s_p_data)
  names(s_p_data)[7] <- "Adj_Close"
  
  s_p_data <- s_p_data %>% mutate(Yesterday = lag(Date))
  
  return(s_p_data)
}


## Mark whether S&P increased or decreased with user specified sensitivity
##Sensitivity = how much of an increase/decrease in order to count as increase/decrease?
## Returns Data Frame with added column 


s_p_direction <- function(s_p_data, sensitivity = 0) {
  s_p_data<-s_p_data %>%
      mutate(Lag_Adj_Close = lag(Adj_Close)) %>%
      mutate(Change = ifelse(
        Adj_Close - Lag_Adj_Close > sensitivity, "H",
        ifelse(
          Adj_Close - Lag_Adj_Close < ( -1 * sensitivity), "L", "S"
        ))
        
    )
  
  return(s_p_data)
}

## Takes S&P Data with marked higher/lower column
## Returns Data Frame with Shading Vectors for use in plotting functions
get_s_p_shading <- function(s_p_data_with_direction){
  
  
  shading_lower_to <- character()
  shading_lower_from <- character()
  
  shading_higher_to <-character()
  shading_higher_from <-character()
  
  shading_same_to <- character()
  shading_same_from <- character()
  
  for(i in 1:nrow(s_p_data_with_direction)) {
    
    if(is.na(s_p_data_with_direction$Change[i])){
      next()
    }
    
    if(s_p_data_with_direction$Change[i] == "H"){
      shading_higher_to <- c(shading_higher_to, as.character(s_p_data_with_direction$Date[i]))
      shading_higher_from <- c(shading_higher_from, as.character(s_p_data_with_direction$Yesterday[i]))
    }
    
    if(s_p_data_with_direction$Change[i]=="L"){
      shading_lower_to <- c(shading_lower_to, as.character(s_p_data_with_direction$Date[i]))
      shading_lower_from <- c(shading_lower_from, as.character(s_p_data_with_direction$Yesterday[i]))
    }
    
    
  }
  
  shading_data <- list(lower_from = c(shading_lower_from), lower_to = c(shading_lower_to), 
                             higher_from = c(shading_higher_from), higher_to = c(shading_higher_to))
  
  
  return(shading_data)
  
}



plot_against_s_p <- function(xts_object, title){}










#