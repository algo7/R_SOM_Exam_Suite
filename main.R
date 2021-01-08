#!/usr/bin/env Rscript
# Load libraries
library(cli)
library(stringr)
library(lpSolve)
library(TTR)


# Misc.:

# File import func
file_import <- function(header) {
  # Import the file
  # filex <- file.choose()
  # Read the file as CSV
  # x <- read.csv(file = filex, header = header)
  x <- read.csv(
    file = "./examples/forecasting/forecasting.csv",
    header = header
  )
  return(x)
}

# Split input func
inp_split <- function(text) {
  result <- strsplit(readline(prompt = text), ",")
  return(result)
}

# Convert to integer func
to_int <- function(list) {
  for (variable in list) {
    int <- as.numeric(variable)
    return(int)
  }
}

# Convert to character func
to_cha <- function(list) {
  for (variable in list) {
    int <- as.character(variable)
    return(int)
  }
}

# Custom lookup func(same as excel lookup)
# Get the value that's smaller or equal to and closest to the input val
look_up <- function(val, to_compare) {
  res <- val - to_compare
  # When overbook no. is 0
  if (length(res) == 0) {
    return(NA)
  } else {
    # Return the position of val. that has the smallest diff from the val
    return(which(res == min(res)))
  }
}

# Prob Type Selection of Continuous Distribution
prob_select <- function() {
  prob_menu <- c(
    "Less Than / At Most | Less Than or Equal to",
    "More Than / At least | More than or Equal to",
    "Between",
    "Probability to Value"
  )
  choice <- menu(prob_menu, title = "Select Relationship Type: ")
  switch(choice,
    "1" = "lt",
    "2" = "mt",
    "3" = "bt",
    "4" = "eq"
  )
}

# Prob Type Selection of Discrete
prob_dis_select <- function() {
  
  prob_dis_menu <- c(
    "Less Than",
    "Less Than or Equal to",
    "More Than",
    "More than or Equal to",
    "Probability to Value"
  )
  choice <- menu(prob_dis_menu, title = "Select Relationship Type: ")
  switch(choice,
    "1" = "lt",
    "2" = "leq",
    "3" = "mt",
    "4" = "meq",
    "5" = "eq"
  )
}


welcome_msg <- "Hi Welcome to the SOM Terminal by Aviv"
cli::cat_boxx(welcome_msg)

# Topic 1 (Forecasting)
# Main Menu List
menu_list_t1 <- c(
  "Simple Moving Average",
  "Weighted Moving Average",
  "Exponential Smoothing",
  "Linear Regression",
  "Error Analysis & Accuracy Comparison",
  "Average Daily Index [Col. No. Must be a Multiple of 7]",
  "Back"
)

# Topic I menu
topic_i <- function() {
  choice <- menu(menu_list_t1, title = "What do you need?")
  switch(choice,
    "1" = {
      sma_func(TRUE)
      cat("\n")
      topic_i()
    },
    "2" = {
      wma_func(TRUE)
      cat("\n")
      topic_i()
    },
    "3" = {
      exp_smooth_func(TRUE)
      cat("\n")
      topic_i()
    },
    "4" = {
      simp_regress(TRUE)
      cat("\n")
      topic_i()
    },
    "5" = {
      err_acc(TRUE)
      cat("\n")
      topic_i()
    },
    "6" = {
      avg_daily_index(TRUE)
      cat("\n")
      topic_i()
    },
    "7" = topic_select()
  )
}

sma_func <- function(print_yes) {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x)
  # Ask for the period to forecast
  sma_period <- to_int(inp_split("Period for SMA e.g.(3,5): "))
  # Calculate the sma for the given period
  sma_val <- TTR::SMA(na.omit(df[, "X.t."]), n = sma_period)
  # Generate the col. name for the sma
  sma_col_name <- paste("SMA.", sma_period, sep = "")
  # Update the df with the sma
  df <- cbind(df, Temp = c(NA, sma_val))
  # Replace the col. names
  colnames(df)[colnames(df) == "Temp"] <- sma_col_name

  if (print_yes == TRUE) {
    # Print the result
    cli::cli_alert_success("Forecasted: ")
    cat("\n")
    print(df)
    cat("\n")
  }

  # Return the predicted column for error analysis
  return(df[as.character(sma_col_name)])
}

wma_func <- function(print_yes) {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x)
  # Ask for the period to forecast
  wma_period <- to_int(inp_split("Period for WMA e.g.(3,5): "))
  # Ask for the weights (must sum up to one)
  wma_wts <- to_int(
    inp_split(
      "Weights for WMA e.g.(0.3,0.5). Enter them as Given,
      the Program will Reverse them for You: "
    )
  )
  # Calculate the wma for the given period
  wma_val <- TTR::WMA(na.omit(df[, "X.t."]), n = wma_period, wts = rev(wma_wts))
  # Generate the col. name for the wma
  wma_col_name <- paste("WMA.", wma_period, sep = "")
  # Update the df with the wma
  df <- cbind(df, Temp = c(NA, wma_val))
  # Replace the col. names
  colnames(df)[colnames(df) == "Temp"] <- wma_col_name

  if (print_yes == TRUE) {
    # Print the result
    cli::cli_alert_success("Forecasted: ")
    cat("\n")
    print(df)
    cat("\n")
  }

  # Return the predicted column for error analysis
  return(df[as.character(wma_col_name)])
}

exp_smooth_func <- function(print_yes) {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x)
  # Ask for the alpha val.
  ses_alpha <- to_int(inp_split("Alpha for SES e.g.(0.3,0.5): "))
  # Replace the NA value with 0 | Only required for SES
  df[, "X.t."][is.na(df[, "X.t."])] <- 0
  # Calculate the ses for the given alpha
  simp_es_val <- vctr::ses(df[, "X.t."], alpha = ses_alpha, initial = "simple")
  # Generate the col. name for the ses
  ses_col_name <- c("SES")
  # Extract the fitted val. and replace the very first one with NA
  fitted <- simp_es_val$fitted
  fitted[1] <- NA
  # Update the df with the ses
  df <- cbind(df, Temp = fitted)
  # Replace the col. names
  colnames(df)[colnames(df) == "Temp"] <- ses_col_name

  if (print_yes == TRUE) {
    # Print the result
    cli::cli_alert_success("Forecasted: ")
    cat("\n")
    print(df)
    cat("\n")
  }

  # Return the predicted column for error analysis
  return(df["SES"])
}

simp_regress <- function(print_yes) {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x)
  # Extract the time and the value columns
  time_col <- colnames(df["t"])
  val_col <- colnames(df["X.t."])
  # Formula Selection Function
  reg_form_select <- function() {
    reg_form_menu <- c(
      opt1 <- paste(time_col, "=", "a + b *", val_col),
      opt2 <- paste(val_col, "=", "a + b *", time_col)
    )
    choice <- menu(reg_form_menu, title = "Select Relationship Type: ")
    switch(choice,
      "1" = formlula <- c(opt1, time_col, val_col),
      "2" = formlula <- c(opt2, val_col, time_col)
    )
  }
  formula <- reg_form_select()
  # The regression formula
  formula_f <- as.formula(paste(formula[2], formula[3], sep = "~"))
  # Generate the model
  lmod <- lm(formula_f, df)
  # The summary
  slmod <- summary(lmod)
  # The coefficients
  slmodc <- slmod$coefficients
  # The final formulas
  text_form <- paste(
    formula[2],
    "=",
    slmodc[formula[3], "Estimate"],
    "*", formula[3], "+", slmodc["(Intercept)", "Estimate"]
  )
  var_form <-
    paste(
      formula[2],
      "=", slmodc[formula[3], "Estimate"],
      "x", "+", slmodc["(Intercept)", "Estimate"]
    )
  # The predicted Values | lmod$fitted.values is missing the last value
  predicted_vals <- df["t"] *
    slmodc[formula[3], "Estimate"] +
    slmodc["(Intercept)", "Estimate"]
  colnames(predicted_vals) <- NULL
  # Update the df with the LR data
  df <- cbind(df, Temp = predicted_vals)
  # Replace the col. names
  colnames(df)[colnames(df) == "Temp"] <- "LR"

  if (print_yes == TRUE) {
    # Print the result
    cli::cli_alert_success("Forecasted: ")
    cat("\n")
    print(df)
    cat("\n")
    cli::cli_alert_info("Formulas: ")
    print(paste("Formula (text):", text_form))
    print(paste("Formula (variable):", var_form))
  }

  # Return the predicted column for error analysis
  return(df["LR"])
}

err_acc <- function(print_yes) {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x)
  # Get the result of each type of prediction methods
  sma_res <- sma_func(FALSE)
  wma_res <- wma_func(FALSE)
  exp_smooth_res <- exp_smooth_func(FALSE)
  simp_regress_res <- simp_regress(FALSE)
  # Combind all the results with the original value
  t_res <- cbind(df["X.t."], sma_res, wma_res, exp_smooth_res, simp_regress_res)
  # Extract the err df
  err_df <- data.frame((t_res[2:length(t_res)] - t_res[, 1])^2)
  # Calculate the mse_res and make a new df out of it
  mse_res <- t(data.frame(colMeans(err_df, na.rm = TRUE)))
  # Update the mse_res col. names with err suffix
  colnames(mse_res) <- paste(colnames(mse_res), ".ERR", sep = "")
  # Update the row names
  rownames(mse_res) <- "mse_res"
  # Calculate the err margin & and combine the MSEs with it
  err_df_final <- data.frame(rbind(mse_res, sqrt(mse_res)))
  # Update the row name
  rownames(err_df_final)[2] <- "EM"
  # Most accurate method
  method_selection_index <- which(err_df_final[2, ] == min(err_df_final))
  best_method <- colnames(err_df_final)[method_selection_index]

  if (print_yes == TRUE) {
    # Print the result
    cat("\n")
    cli::cli_alert_success("Results: ")
    cat("\n")
    print(err_df_final)
    cat("\n")
    print(paste(
      "Most Accurate Method:",
      stringr::str_remove(best_method, ".ERR")
    ))
    cat("\n")
    cli::cli_alert_success("Original Result:")
    print(t_res)
    cat("\n")
  }

  # Return Err. df for analysis
  return(err_df_final)
}

avg_daily_index <- function(print_yes) {

  # Prediction Methods List
  pred_menu <- c(
    "Simple Moving Average",
    "Weighted Moving Average",
    "Exponential Smoothing",
    "Linear Regression"
  )

  # Prediction Methods Menu
  pred_select <- function() {
    choice <- menu(pred_menu, title = "Which Method do You Want to Use?")
    switch(choice,
      "1" = return(sma_func(FALSE)),
      "2" = return(wma_func(FALSE)),
      "3" = return(exp_smooth_func(FALSE)),
      "4" = return(simp_regress(FALSE))
    )
  }
  # Get the predicted vals from the selected optimization methods
  predicted_val <- pred_select()
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x)
  # Add the predicted result
  df <- cbind(df, predicted_val)
  # Get the index of the first NA val., which indicates today / this month
  today_index <- which(is.na(df[, "X.t."]))[1] - 1
  # Range of existing data (no forecast | current observation)
  exidr <- c(1:today_index)
  # Calculate the daily index
  daily_index <- (df[, "X.t."][exidr] - df[, "LR"][exidr]) / df[, "LR"][exidr]
  # Convert it to df
  daily_index <- data.frame(daily_index)
  # Padding
  daily_index[(today_index + 1):length(df[, "t"]), ] <- NA

  # Date / Month Switch
  month_day_switch <- to_int(inp_split("Is the Cycle Month=1 or Date=2: "))

  if (month_day_switch == 1) {
    # Bind the daily_index into the df & re-name the col.
    colnames(daily_index) <- NULL
    df <- cbind(df, monthlyIndex = daily_index)
    # Create the avg. monthly index table
    avg_mi_mat <- matrix(NA, nrow = 12, ncol = 1)
    # Convert it to df
    avg_mi_mat <- data.frame(avg_mi_mat)
    # Assign row & col. names
    colnames(avg_mi_mat) <- c("Avg.Monthly.Index")
    rownames(avg_mi_mat) <- c(
      "January", "February",
      "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"
    )
    # Cal. the avg.
    for (i in seq_len(length(rownames(avg_mi_mat)))) {
      # Find the index of each re-occurring weekdays
      index <- which(df[, "Month"][exidr] == rownames(avg_mi_mat)[i])
      # Update the avg. daily index df
      avg_mi_mat[, "Avg.Monthly.Index"][i] <- mean(df[, "monthlyIndex"][index])
    }
    # Cal. LR+SI
    temp_si <- data.frame(rep.int(
      unlist(avg_mi_mat[1]),
      ceiling(length(df[, 1]) / 12)
    ))
    # Calculate the length diff
    l_temp_si <- length(tempSI[, 1])
    l_dff <- l_temp_si - length(df[, "t"])
    # tem_siis longer than the df
    if (sign(l_dff) == 1) {
      l_dff <- l_dff - 1
      temp_si <- data.frame(tempSI[, 1][-((l_temp_si - l_dff):l_temp_si)])

      # tem_siis shorter than the df
    } else if (sign(l_dff) == -1) {
      paddings <- unlist(avg_mi_mat[1])[1:abs(l_dff)]
      tempSI[l_temp_si:length(df[, "t"]), ] <- as.numeric(paddings)
    }
  } else {
    # Bind the daily_index into the df
    df <- cbind(df, daily_index)
    # Create the avg. daily index table
    avg_di_mat <- matrix(NA, nrow = 7, ncol = 1)
    # Convert it to df
    avg_di_mat <- data.frame(avg_di_mat)
    # Assign row & col. names
    colnames(avg_di_mat) <- c("Avg.Daily.Index")
    rownames(avg_di_mat) <-
      c(
        "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
      )
    # Cal. the avg.
    for (i in seq_len(length(rownames(avg_di_mat)))) {
      # Find the index of each re-occurring weekdays
      index <- which(df[, "Day"][exidr] == rownames(avg_di_mat)[i])
      # Update the avg. daily index df
      avg_di_mat[, "Avg.Daily.Index"][i] <- mean(df[, "daily_index"][index])
    }
    # Cal. LR+SI
    temp_si <- data.frame(rep.int(unlist(avg_di_mat[1]), length(df[, 1]) / 7))
    # Calculate the length diff
    l_temp_si <- length(tempSI[, 1])
    l_dff <- l_temp_si - length(df[, "t"])
    # tem_siis longer than the df
    if (sign(l_dff) == 1) {
      l_dff <- l_dff - 1
      temp_si <- data.frame(tempSI[, 1][-((l_temp_si - l_dff):l_temp_si)])
      # tem_siis shorter than the df
    } else if (sign(l_dff) == -1) {
      paddings <- unlist(avg_di_mat[1])[1:abs(l_dff)]
      tempSI[l_temp_si:length(df[, "t"]), ] <- as.numeric(paddings)
    }
  }

  colnames(tempSI) <- NULL
  # Bind the SI into the df
  df <- cbind(df, temp_si = tempSI)
  # Calculate LR+SI
  df[, "tempSI"] <- df[, "LR"] + df[, "LR"] * df[, "tempSI"]
  # Replace the col. names
  colnames(df)[colnames(df) == "tempSI"] <- "LR+SI"
  # Cal. mse_res till last observation
  mse_lr <- (df[, "LR"][exidr] - df[, "X.t."][exidr])^2
  mse_lr_si <- (df[, "LR+SI"][exidr] - df[, "X.t."][exidr])^2
  # Convert to dfs
  mse_lr <- data.frame(mse_lr)
  mse_lr_si <- data.frame(mse_lr_si)
  # Bind them all
  t_mse <- cbind(mse_lr, mse_lr_si)
  # Cal. ME as well
  err_df <- rbind(colMeans(t_mse), sqrt(colMeans(t_mse)))
  # Update the row name
  rownames(err_df) <- c("mse_res", "ME")
  # Most accurate method
  best_method <- colnames(err_df)[which(err_df[2, ] == min(err_df))]

  if (print_yes == TRUE) {
    # Print the result
    cat("\n")
    cli::cli_alert_success("Results: ")
    cat("\n")
    print(err_df)
    cat("\n")
    print(
      paste(
        "Most Accurate Method:",
        stringr::str_remove(best_method, ".ERR")
      )
    )
    cat("\n")
    cli::cli_alert_success("Original Result:")
    print(df)
    cat("\n")
  }
}


# Topic 2 (Probability Distribution)
# Main Menu List
menu_list_t2 <- c(
  "Normal Distribution",
  "Exponential Distribution (Time Between Occurrences. eg.g Waiting TIme)",
  "Poisson Distribution (No. of Occurrences)",
  "Linear Regression",
  "Error Analysis & Accuracy Comparison",
  "Average Daily Index [Col. No. Must be a Multiple of 7]",
  "Back"
)

# Topic II menu
topic_ii <- function() {
  choice <- menu(menu_list_t2, title = "What do you need?")
  switch(choice,
    "1" = {
      norm_distro()
      cat("\n")
      topic_ii()
    },
    "2" = {
      exp_distro()
      cat("\n")
      topic_ii()
    },
    "3" = {
      poisson_distro()
      cat("\n")
      topic_ii()
    },
    "4" = {
      simp_regress(TRUE)
      cat("\n")
      topic_ii()
    },
    "5" = {
      err_acc(TRUE)
      cat("\n")
      topic_ii()
    },
    "6" = {
      avg_daily_index(TRUE)
      cat("\n")
      topic_ii()
    },
    "7" = topic_select()
  )
}

norm_distro <- function() {

  # Determin type
  select_res <- prob_select()

  if (identical(select_res, "lt")) {

    # Read the input
    info <- to_int(inp_split("Enter (Value,Mean,Stdev) in CSV: "))
    p <- pnorm(info[1], info[2], info[3])
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_res, "mt")) {

    # Read the input
    info <- to_int(inp_split("Enter (Value,Mean,Stdev) in CSV: "))
    p <- pnorm(info[1], info[2], info[3], lower.tail = FALSE)
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_res, "bt")) {

    # Read the input
    info <- to_int(
      inp_split("Enter (Smaller Value, Larger Value,Mean,Stdev) in CSV: ")
    )
    # Smaller val.
    p1 <- pnorm(info[1], info[3], info[4])
    # Large val.
    p2 <- pnorm(info[2], info[3], info[4])
    p3 <- p2 - p1
    cat("\n")
    print(paste("The Probability is: ", p3))
    cat("\n")
  } else if (identical(select_res, "eq")) {

    # Ask for the type of probabilities
    prob_type <- readline(prompt = "Less Than / At Most (lt) | More Than / At Least (mt): ")
    # Prob type switchs
    if (identical(prob_type, "lt")) {

      # Read the input
      info <- to_int(inp_split("Enter (Prbability,Mean,Stdev) in CSV: "))
      val <- qnorm(info[1], info[2], info[3])
      cat("\n")
      print(paste("The value is: ", val))
      cat("\n")
    } else if (identical(prob_type, "mt")) {

      # Read the input
      info <- to_int(inp_split("Enter (Prbability,Mean,Stdev) in CSV: "))
      val <- qnorm(info[1], info[2], info[3], lower.tail = FALSE)
      cat("\n")
      print(paste("The value is: ", val))
      cat("\n")
    }
  }
}

exp_distro <- function() {

  # Determin type
  select_res <- prob_select()

  # Exponential distro (cal. interval) | (rate = service or product / min)
  if (identical(select_res, "lt")) {
    # Read the input
    info <- to_int(inp_split("Enter (Value, Mean) in CSV: "))
    p <- pexp(info[1], 1 / info[2])
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_res, "mt")) {
    # Read the input
    info <- to_int(inp_split("Enter (Value, Mean) in CSV: "))
    p <- pexp(info[1], 1 / info[2], lower.tail = FALSE) # More than
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_res, "bt")) {
    # Read the input
    info <- to_int(
      inp_split("Enter (Smaller Value, Larger Value, Mean) in CSV: ")
    )
    # Smaller val.
    p1 <- pexp(info[1], 1 / info[3])
    # Large val.
    p2 <- pexp(info[2], 1 / info[3])
    p3 <- p2 - p1
    cat("\n")
    print(paste("The Probability is: ", p3))
    cat("\n")
  } else if (identical(select_res, "eq")) {
    # Ask for the type of probabilities
    prob_type <- readline(prompt = " Probability: At most: P[X ≤ x] (default) or More than: P[X > x] (>)")
    # Prob type switchs
    if (identical(prob_type, "")) {
      # Read the input
      info <- to_int(inp_split("Enter (Prbability, Mean) in CSV: "))
      val <- qexp(info[1], 1 / info[2])
      cat("\n")
      print(paste("The value is: ", val))
      cat("\n")
    } else {
      # Read the input
      info <- to_int(inp_split("Enter (Prbability, Mean) in CSV: "))
      val <- qexp(info[1], 1 / info[2], lower.tail = FALSE)
      cat("\n")
      print(paste("The value is: ", val))
      cat("\n")
    }
  }

  # Determin the type of operation.
}

poisson_distro <- function() {

  # Determin type
  select_dis_res <- prob_dis_select()

if (identical(select_dis_res,"lt")) {

}else if (identical(select_dis_res,"leq")) {

}else if (identical(select_dis_res,"mt"))


}else if (identical(select_dis_res,"meq"))




# Main Menu Selection Function
topic_select <- function() {
  menu_list <- c(
    "Forecasting",
    "Probability Distribution"
  )

  choice <- menu(menu_list, title = "Please Select A Topic:")
  # Menu Selection Function
  m_select <- function(topic) {
    switch(topic,
      "1" = topic_i(),
      "2" = topic_ii()
    )
  }
  m_select(choice)
}
topic_select()