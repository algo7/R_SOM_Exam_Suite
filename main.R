#!/usr/bin/env Rscript
# Load libraries
library(cli)
library(stringr)
library(lpSolve)
library(TTR)
library(AnalyzeTS)

# Misc.:

# File import func
fileImport<-function(header){
  # Import the file
  filex<-file.choose()
  # Read the file as CSV
  x<-read.csv(file=filex,header = header)
  return (x)
}

# Split input func
inpSplit<-function(text){
  result<- strsplit(readline(prompt=text),",")
  return(result)
}

# Convert to integer func
toInt<-function(list){
  for (variable in list) {
    int<-as.numeric(variable)
    return(int)
  }
}

# Convert to character func
toCha<-function(list){
  for (variable in list) {
    int<-as.character(variable)
    return(int)
  }
}

# Custom lookup func(same as excel lookup)
# Get the value that's smaller or equal to and closest to the input val
lookUp<-function(val,toCompare){
  res<-val-toCompare
  # When overbook no. is 0
  if(length(res)==0){
    return(NA)
  }else{
    # Return the position of val. that has the smallest diff from the val
    return(which(res==min(res)))
  }

}


welcomeMsg<-'Hi Welcome to the SOM Terminal by Aviv'
cli::cat_boxx(welcomeMsg)

# Topic 1 (Forecasting)
# Main Menu List
menuListT1<-c(
  'Simple Moving Average',
  'Weighted Moving Average',
  'Exponential Smoothing',
  'Back'
)

# Topic I menu
topicI<-function(){
  choice<-menu(menuListT1,title='What do you need?')
  switch (choice,
          '1' = {smaFunc(); cat('\n');topicI()},
          '2' = {wmaFunc();cat('\n');topicI()},
          '3' = {expSmoothFunc();cat('\n');topicI()}
          '4'=topicSelect()
  )
}

smaFunc<-function(){
  # Import the file
  x<-fileImport(TRUE)
  #Convert it to df
  df<-data.frame(x)
  # Ask for the period to forecast
  smaPeriod<-toInt(inpSplit('Period for SMA e.g.(3,5): '))
  # Calculate the sma for the given period
  smaVal<-SMA(na.omit(df[,'X.t.']),n=smaPeriod)
  # Generate the col. name for the sma
  smaColName<-paste('SMA.',smaPeriod,sep='')
  # Update the df with the sma
  df<-cbind(df,Temp=c(NA,smaVal))
  # Replace the col. names
  colnames(df)[colnames(df)=='Temp']<-smaColName

  # Print the result
  cli_alert_success('Forecasted: ')
  cat('\n')
  print(df)
  cat('\n')
}

wmaFunc<-function(){
  # Import the file
  x<-fileImport(TRUE)
  #Convert it to df
  df<-data.frame(x)
  # Ask for the period to forecast
  wmaPeriod<-toInt(inpSplit('Period for WMA e.g.(3,5): '))
  # Ask for the weights (must sum up to one)
  wmaWts<-toInt(inpSplit('Weights for WMA e.g.(0.3,0.5). Enter them as Given, the Program will Reverse them for You: '))
  # Calculate the wma for the given period
  wmaVal<-WMA(na.omit(df[,'X.t.']),n=wmaPeriod,wts=rev(wmaWts))
  # Generate the col. name for the wma
  wmaColName<-paste('WMA.',wmaPeriod,sep='')
  # Update the df with the wma
  df<-cbind(df,Temp=c(NA,wmaVal))
  # Replace the col. names
  colnames(df)[colnames(df)=='Temp']<-wmaColName

  # Print the result
  cli_alert_success('Forecasted: ')
  cat('\n')
  print(df)
  cat('\n')
}

expSmoothFunc<-function(){
  # Import the file
  x<-fileImport(TRUE)
  #Convert it to df
  df<-data.frame(x)
  # Ask for the alpha val.
  sesAlpha<-toInt(inpSplit('Alpha for SES e.g.(0.3,0.5): '))
  # Replace the NA value with 0 | Only required for SES
  df[,'X.t.'][is.na(df[,'X.t.'])]<-0
  # Calculate the ses for the given alpha
  sesVal<-ses(df[,'X.t.'],alpha=sesAlpha,initial = 'simple')
  # Generate the col. name for the ses
  sesColName<-c('SES')
  # Extract the fitted val. and replace the very first one with NA
  fitted<-sesVal$fitted
  fitted[1]<-NA
  # Update the df with the ses
  df<-cbind(df,Temp=fitted)
  # Replace the col. names
  colnames(df)[colnames(df)=='Temp']<-sesColName

  # Print the result
  cli_alert_success('Forecasted: ')
  cat('\n')
  print(df)
  cat('\n')

}





# Main Menu Selection Function
topicSelect=function(){
  menuList<-c(
    'Forecasting'
  );

  choice<-menu(menuList, title='Please Select A Topic:');
  # Menu Selection Function
  mSelect<-function(topic){
    switch (topic,
            '1' = topicI(),
    )
  };
  mSelect(choice);
}
topicSelect()
