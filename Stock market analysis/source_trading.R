# Main function showing one plot of stock index from 0 to 100 with 12 month regression
plotStock <- function(stock){
  require(quantmod)    
  stock <- toupper(stock)
  today = Sys.Date()+1
  yday = today - 730
  todayStr = as.character(today)
  ydayStr = as.character(yday)
  
  stockTable <- getSymbols(stock, from = ydayStr , to = todayStr , auto.assign = FALSE)
  df <- stockTable[,4]
  
  a = c()
  current = 0
  max1 = 0
  min1 = 0
  out = 0
  for(i in 1:length(df)){
    if(i < 252){
      a = append(a, NA)
    }
    else{
      temp = df[(i-252):i]
      max1 = max(temp)
      min1 = min(temp)
      current = df[i]
      out = ((current - min1)/(max1-min1))*100
      a = append(a,out)
      
    }
    
  }
  
  
  plot(a[253:length(a)], type = 'l', ylab = 'Index', xlab = 'Day', main = stock, lwd = 2, 
       col = '#FF7315')
  grid(6, 10, lwd = 1, lty=1)
  
}

# function for list of stocks
plotStocks <- function(stock_list){
  for(i in stock_list){
    plotStock(i)
  }
}

# show actual graph of single stock
plotStockNominal <- function(stock){
  require(quantmod)    
  stock <- toupper(stock)
  today = Sys.Date()+1
  yday = today - 365
  todayStr = as.character(today)
  ydayStr = as.character(yday)
  
  stockTable <- getSymbols(stock, from = ydayStr , to = todayStr , auto.assign = FALSE)
  df <- stockTable[,4]
  
  temp <- c()
  for(i in df[,1]){
    temp = append(temp, i)
  }
  closing_price = temp[length(temp)]
  last_closing = temp[length(temp)-1]
  change_percent = ((closing_price - last_closing)*100)/last_closing
  lab <- paste(stock,' $',  round(closing_price,2),' ',round(change_percent,2), '%')
  #plot(temp , type = 'l', ylab = 'Price', main = lab)
  plot(temp, type = 'l', ylab = 'Price', xlab = 'Day', main = lab, lwd = 2, col = '#3fc5f0',
       col.main = 'purple')
  grid(6,15, lwd = 1, lty=1)
}


# function to plot nominal stocks from list
plotMultiple <- function(stock_list){
  for(i in stock_list){
    plotStockNominal(i)
  }
}


#This function plots index graphs for 25 stocks at a time from monitor.csv
look25 <- function(num){
  look <- read.csv('monitor.csv')
  look$Ticker <- as.character(look[,1])
  part <- num*25
  stock_list <- look$Ticker[(part-24):part]
  for(i in stock_list){
    plotStock(i)
  }
}

#This function plots nominal graphs for 25 stocks at a time from monitor.csv
lookNominal25 <- function(num){
  look <- read.csv('monitor.csv')
  look$Ticker <- as.character(look[,1])
  part <- num*25
  stock_list <- look$Ticker[(part-24):part]
  for(i in stock_list){
    plotStockNominal(i)
  }
}


# read xls and calculate last price to compare

# This is function to get last price.
# currently it gives close price of last day
# I want it to show real time price and I am working on it.
last_price <- function(stock){
  require(quantmod)    
  stock <- toupper(stock)
  today = Sys.Date()+1
  yday = today - 4
  todayStr = as.character(today)
  ydayStr = as.character(yday)
  
  stockTable <- getSymbols(stock, from = ydayStr , to = todayStr , auto.assign = FALSE)
  
  out <- stockTable[nrow(stockTable),4]
  return(as.numeric(out))  
}

performance <- function(){
  require(dplyr)
  setwd("C:/Users/aniruddha/Desktop/Data Science/Trading")
  my_stocks <- read.csv('stocks.csv', header = TRUE)
  my_stocks <- na.omit(my_stocks)
  out_vector = c()
  for(i in 1:nrow(my_stocks)){
    temp = last_price(as.character(my_stocks[i,2]))
    
    out_vector <- append(out_vector, round(temp, 2))
  }
  my_stocks['current'] <- out_vector
  my_stocks <-  my_stocks %>% mutate(profit = paste(round((current-Price)*100/Price,2), ' %')) 
  
  write.csv(my_stocks, file = "stock_performance.csv")
  
  my_stocks[,c(1,2,6,3,4,5)]
}





# This is to run stocks from s&p500 list, 25 at a time
plotNominalBlock <- function(number){
  sp500 <- read.csv('sp500.csv')
  sp500$Symbol <- as.character(sp500$Symbol)
  num <- number*25
  sp500Block <- sp500[(num-24):num,1]
  for(i in sp500Block){
    plotStockNominal(i)
  }
}


# This is to run stocks from sp500 list, producing percent plots, 25 at a time
plotBlock <- function(number){
  sp500 <- read.csv('sp500.csv')
  sp500$Symbol <- as.character(sp500$Symbol)
  num <- number*25
  sp500Block <- sp500[(num-24):num,1]
  for(i in sp500Block){
    plotStock(i)
  }
}

