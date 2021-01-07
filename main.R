#!/usr/bin/env Rscript
# Load libraries
library(cli)
library(stringr)
library(lpSolve)
library(TTR)

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
  'Linear Regression',
  'Error Analysis & Accuracy Comparison',
  'Average Daily Index [Col. No. Must be a Multiple of 7]',
  'Back'
)

# Topic I menu
topicI<-function(){
  choice<-menu(menuListT1,title='What do you need?')
  switch (choice,
          '1' = {smaFunc(TRUE);cat('\n');topicI()},
          '2' = {wmaFunc(TRUE);cat('\n');topicI()},
          '3' = {expSmoothFunc(TRUE);cat('\n');topicI()},
          '4' = {simpRegress(TRUE);cat('\n');topicI()},
          '5' = {errAAC(TRUE);cat('\n');topicI()},
          '6' = {avgDailyIndex(TRUE);cat('\n');topicI()},
          '7' = topicSelect()
  )
}

smaFunc<-function(printYes){
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

  if(printYes==TRUE){
    # Print the result
    cli_alert_success('Forecasted: ')
    cat('\n')
    print(df)
    cat('\n')
  }

  # Return the predicted column for error analysis
  return(df[as.character(smaColName)])

}

wmaFunc<-function(printYes){
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

  if(printYes==TRUE){
    # Print the result
    cli_alert_success('Forecasted: ')
    cat('\n')
    print(df)
    cat('\n')
  }

  # Return the predicted column for error analysis
  return(df[as.character(wmaColName)])
}

expSmoothFunc<-function(printYes){
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

  if(printYes==TRUE){
    # Print the result
    cli_alert_success('Forecasted: ')
    cat('\n')
    print(df)
    cat('\n')
  }

  # Return the predicted column for error analysis
  return(df['SES'])

}

simpRegress<-function(printYes){
  # Import the file
  x<-fileImport(TRUE)
  #Convert it to df
  df<-data.frame(x)
  # Extract the time and the value columns
  timeCol<-colnames(df['t'])
  valCol<-colnames(df['X.t.'])
  # Formula Selection Function
  regFormSelect<-function(){
    formlula<-character()
    regFormMenu<-c(
      opt1<-paste(timeCol,'=','a + b *',valCol),
      opt2<-paste(valCol,'=','a + b *',timeCol)
    );
    choice<-menu(regFormMenu,title='Select Relationship Type: ')
    switch (choice,
            '1' = formlula<-c(opt1,timeCol,valCol),
            '2' = formlula<-c(opt2,valCol,timeCol)
    )
  }
  formula<-regFormSelect()
  # The regression formula
  formulaF<-as.formula(paste(formula[2],formula[3],sep = '~'))
  # Generate the model
  lmod<-lm(formulaF,df)
  # The summary
  slmod<-summary(lmod)
  # The coefficients
  slmodc<-slmod$coefficients
  # The final formulas
  textForm<-paste(formula[2],'=',slmodc[formula[3],'Estimate'],'*',formula[3],'+',slmodc['(Intercept)','Estimate'])
  varForm<-
    paste(formula[2],'=',slmodc[formula[3],'Estimate'],'x','+',slmodc['(Intercept)','Estimate'])
  # The predicted Values | lmod$fitted.values is missing the last value
  predictedVals<-df['t']*slmodc[formula[3],'Estimate']+slmodc['(Intercept)','Estimate']
  colnames(predictedVals)<-NULL
  # Update the df with the LR data
  df<-cbind(df,Temp=predictedVals)
  # Replace the col. names
  colnames(df)[colnames(df)=='Temp']<-'LR'

  if(printYes==TRUE){
    # Print the result
    cli_alert_success('Forecasted: ')
    cat('\n')
    print(df)
    cat('\n')
    cli_alert_info('Formulas: ')
    print(paste('Formula (text):',textForm))
    print(paste('Formula (variable):',varForm))
  }

  # Return the predicted column for error analysis
  return(df['LR'])

}

errAAC<-function(printYes){
  # Import the file
  x<-fileImport(TRUE)
  #Convert it to df
  df<-data.frame(x)
  # Get the result of each type of prediction methods
  smaRes<-smaFunc(FALSE)
  wmaRes<-wmaFunc(FALSE)
  expSmoothRes<-expSmoothFunc(FALSE)
  simpRegressRes<-simpRegress(FALSE)
  # Combind all the results with the original value
  tRes<-cbind(df['X.t.'],smaRes,wmaRes,expSmoothRes,simpRegressRes)
  # Extract the err df
  errDf<-data.frame((tRes[2:length(tRes)]-tRes[,1])^2)
  # Calculate the MSE and make a new df out of it
  MSE<-t(data.frame(colMeans(errDf,na.rm = TRUE)))
  # Update the MSE col. names with err suffix
  colnames(MSE)<-paste(colnames(MSE),'.ERR',sep = '')
  # Update the row names
  rownames(MSE)<-'MSE'
  # Calculate the err margin & and combine the MSEs with it
  errDfFinal<-data.frame(rbind(MSE,sqrt(MSE)))
  # Update the row name
  rownames(errDfFinal)[2]<-'EM'
  # Most accurate method
  bestMethod<-colnames(errDfFinal)[which(errDfFinal[2,]==min(errDfFinal))]

  if(printYes==TRUE){
    # Print the result
    cat('\n')
    cli_alert_success('Results: ')
    cat('\n')
    print(errDfFinal)
    cat('\n')
    print(paste('Most Accurate Method:',str_remove(bestMethod, '.ERR')))
    cat('\n')
    cli_alert_success('Original Result:')
    print(tRes)
    cat('\n')
  }

  # Return Err. df for analysis
  return(errDfFinal)

}

avgDailyIndex<-function(printYes){

  # Prediction Methods List
  predMenu<-c(
    'Simple Moving Average',
    'Weighted Moving Average',
    'Exponential Smoothing',
    'Linear Regression'
  )

  # Prediction Methods Menu
  predSelect<-function(){
    choice<-menu(predMenu,title='Which Method do You Want to Use?')
    switch (choice,
            '1' = return(smaFunc(FALSE)),
            '2' = return(wmaFunc(FALSE)),
            '3' = return(expSmoothFunc(FALSE)),
            '4' = return(simpRegress(FALSE))
    )
  }
  # Get the predicted vals from the selected optimization methods
  predictedVal<-predSelect()
  # Import the file
  x<-fileImport(TRUE)
  # Convert it to df
  df<-data.frame(x)
  # Add the predicted result
  df<-cbind(df,predictedVal)
  # Get the index of the first NA val., which indicates today
  todayIndex<-which(is.na(df[,'X.t.']))[1]-1
  # Range of existing data (no forecast)
  exiDR<-c(1:todayIndex)
  # Calculate the daily index
  dailyIndex<-(df[,'X.t.'][exiDR]-df[,'LR'][exiDR])/df[,'LR'][exiDR]
  # Convert it to df
  dailyIndex<-data.frame(dailyIndex)
  dailyIndex[(todayIndex+1):length(df[,'t']),]<-NA
  # Bind the dailyIndex into the df
  df<-cbind(df,dailyIndex)
  # Create the avg. daily index table
  avgDIMat<-matrix(NA, nrow=7,ncol=1)
  # Convert it to df
  avgDIMat<-data.frame(avgDIMat)
  # Assign row & col. names
  colnames(avgDIMat)<-c('Avg.Daily.Index')
  rownames(avgDIMat)<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  # Cal. the avg.
  for (i in 1:length(rownames(avgDIMat))) {
    # Find the index of each re-occurring weekdays
    index<-which(df[,'Day'][exiDR]==rownames(avgDIMat)[i])
    # Update the avg. daily index df
    avgDIMat[,'Avg.Daily.Index'][i]<-mean(df[,'dailyIndex'][index])
  }
  # Cal. LR+SI
  tempSI<-data.frame(rep.int(unlist(avgDIMat[1]),length(df[,1])/7))
  colnames(tempSI)<-NULL
  # Bind the SI into the df
  df<-cbind(df,tempSI=tempSI)
  # Calculate LR+SI
  df[,'tempSI']<-df[,'LR']+df[,'LR']*df[,'tempSI']
  # Replace the col. names
  colnames(df)[colnames(df)=='tempSI']<-'LR+SI'
  # Cal. MSE till last observation
  mseLR<-(df[,'LR'][exiDR]-df[,'X.t.'][exiDR])^2
  mseLRSI<-(df[,'LR+SI'][exiDR]-df[,'X.t.'][exiDR])^2
  # Convert to dfs
  mseLR<-data.frame(mseLR)
  mseLRSI<-data.frame(mseLRSI)
  # Bind them all
  tMSE<-cbind(mseLR,mseLRSI)
  # Cal. ME as well
  errDf<-rbind(colMeans(tMSE),sqrt(colMeans(tMSE)))
  # Update the row name
  rownames(errDf)<-c('MSE','ME')
  # Most accurate method
  bestMethod<-colnames(errDf)[which(errDf[2,]==min(errDf))]

  if(printYes==TRUE){
    # Print the result
    cat('\n')
    cli_alert_success('Results: ')
    cat('\n')
    print(errDf)
    cat('\n')
    print(paste('Most Accurate Method:',str_remove(bestMethod, '.ERR')))
    cat('\n')
    cli_alert_success('Original Result:')
    print(df)
    cat('\n')
  }


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
