#!/usr/bin/env Rscript
# Load libraries
library(cli)
library(stringr)
library(lpSolve)
library(TTR)
library(openxlsx)
library(XLConnect)
library(ggplot2)
library(hrbrthemes)

# Misc.:
# File import func
file_import <- function(header) {
  # Import the file
  # filex <- file.choose()
  # Read the file as CSV
  # x <- read.csv(file = filex, header = header)
  x <- read.csv(
    file = "./examples/inventory_management/inv_mgmt.csv",
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
# Disable scientific notation
options(scipen = 999)

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
    "Exactly Equal"
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
  "Average Daily Index",
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
    "4" = topic_select()
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

  # Custom Lambda (mean wont get divided by 1)
  custom_lambda <- to_int(inp_split("Custom Lambda? (Yes=1, No=0): "))

  # Exponential distro (cal. interval) | (rate = service or product / min)
  if (identical(select_res, "lt")) {
    # Read the input
    info <- to_int(inp_split("Enter (Value, Mean) in CSV: "))
    if (custom_lambda == 1) {
      p <- pexp(info[1], info[2])
    } else {
      p <- pexp(info[1], 1 / info[2])
    }
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_res, "mt")) {
    # Read the input
    info <- to_int(inp_split("Enter (Value, Mean) in CSV: "))
    if (custom_lambda == 1) {
      p <- pexp(info[1], info[2], lower.tail = FALSE)
    } else {
      p <- pexp(info[1], 1 / info[2], lower.tail = FALSE)
    }
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_res, "bt")) {
    # Read the input
    info <- to_int(
      inp_split("Enter (Smaller Value, Larger Value, Mean) in CSV: ")
    )
    if (custom_lambda == 1) {
      # Smaller val.
      p1 <- pexp(info[1], info[3])
      # Large val.
      p2 <- pexp(info[2], info[3])
      p3 <- p2 - p1
    } else {
      # Smaller val.
      p1 <- pexp(info[1], 1 / info[3])
      # Large val.
      p2 <- pexp(info[2], 1 / info[3])
      p3 <- p2 - p1
    }
    cat("\n")
    print(paste("The Probability is: ", p3))
    cat("\n")
  } else if (identical(select_res, "eq")) {
    # Ask for the type of probabilities
    prob_type <- readline(prompt = "Less Than / At Most (lt) | More Than / At Least (mt): ")
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
}

poisson_distro <- function() {
  # Determin type
  select_dis_res <- prob_dis_select()
  # Prob type switchs
  if (identical(select_dis_res, "lt")) {

    # Read the input
    info <- to_int(inp_split("Enter (Value, Lambda) in CSV: "))
    p <- ppois(info[1], info[2])
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_dis_res, "leq")) {
    # Read the input
    info <- to_int(inp_split("Enter (Value, Lambda) in CSV: "))
    p <- ppois(info[1] + 1, info[2])
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_dis_res, "mt")) {

    # Read the input
    info <- to_int(inp_split("Enter (Value, Lambda) in CSV: "))
    p <- ppois(info[1], info[2], lower.tail = FALSE)
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_dis_res, "meq")) {

    # Read the input
    info <- to_int(inp_split("Enter (Value, Lambda) in CSV: "))
    p <- ppois(info[1] - 1, info[2], lower.tail = FALSE)
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  } else if (identical(select_dis_res, "eq")) {
    # Read the input
    info <- to_int(inp_split("Enter (Value, Lambda) in CSV: "))
    p <- dpois(info[1], info[2])
    cat("\n")
    print(paste("The Probability is: ", p))
    cat("\n")
  }
}


# Topic 3 (Process Analysis)
# Main Menu List
menu_list_t3 <- c(
  "Process Analysis",
  "Back"
)

# Topic III menu
topic_iii <- function() {
  choice <- menu(menu_list_t3, title = "What do you need?")
  switch(choice,
    "1" = {
      process_analysis()
      cat("\n")
      topic_iii()
    },
    "2" = topic_select()
  )
}

process_analysis <- function() {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x)
  # Calculate the capacity
  df[, "Capacity"] <- df[, "Resource"] / df[, "Time"]
  # Unit to times the capacity by
  capacity_fac <- to_int(inp_split("Factor to Multiply the Capacity by (60,120,1): "))
  df[, "Capacity"] <- df[, "Capacity"] * capacity_fac
  # The bottleneck
  bottleneck_index <- which(df[, "Capacity"] == min(df[, "Capacity"]))
  bottleneck <- df[, "Task.Name"][bottleneck_index]
  # Calculate
  cycle_time <- sum(df[, "Time"])
  # Merging tasks does not change capacity but the waiting time for the "Customers"
  # Adding resources increase the " Capacity" but not the "Time"
  # Changing the time will only affect the "overall capacity" if it affects / create new bottleneck
  # Remeber the resource type. It might not be people all the time, it can be the tools as well
  # Write to CSV
  write.csv(df, "./calculated.csv", row.names = FALSE)

  # Print the result
  cat("\n")
  cli::cli_alert_success("Results: ")
  cat("\n")
  print(paste("Cycle Time:", cycle_time))
  cat("\n")
  print(paste("Bottleneck:", bottleneck))
  cat("\n")
}

# Topic 4 (Waiting Lines)
# Main Menu List
menu_list_t4 <- c(
  "MMK Table",
  "Back"
)

# Topic IV menu
topic_iv <- function() {
  choice <- menu(menu_list_t4, title = "What do you need?")
  switch(choice,
    "1" = {
      waiting_lines()
      cat("\n")
      topic_iv()
    },
    "2" = topic_select()
  )
}

waiting_lines <- function() {
  # Load workbook
  wb <- XLConnect::loadWorkbook("./examples/waiting_lines/mmk.xlsx")
  # Ask for Arrival rate: λ, Service rate: μ = 60, Service Point Count = 1 (mm1), 2 (mmk)
  params <- to_int(inp_split("Enter Arrival rate = λ, Service rate = μ, Service Point Count = k (e.g. 60,20,2): "))
  # Arrival rate: λ
  XLConnect::writeWorksheet(wb, "mmk", data = params[1], startCol = 2, startRow = 2, header = FALSE)
  # Service rate: μ = 60,
  XLConnect::writeWorksheet(wb, "mmk", data = params[2], startCol = 2, startRow = 3, header = FALSE)
  # Service Point Count = 1 (mm1), 2 (mmk)
  XLConnect::writeWorksheet(wb, "mmk", data = params[3], startCol = 2, startRow = 4, header = FALSE)
  # Force formula recalculation
  XLConnect::setForceFormulaRecalculation(wb, sheet = 1, TRUE)
  # Read the workbook for the recalculation to take place
  temp <- XLConnect::readWorksheet(wb, "mmk")
  temp <- temp # Avoid linter warning of unused var
  # Update the workbook on the disk
  XLConnect::saveWorkbook(wb, "./examples/waiting_lines/mmk1.xlsx")
  # Load the updated workbook
  df <- openxlsx::loadWorkbook("./examples/waiting_lines/mmk1.xlsx")
  df <- openxlsx:::readWorkbook(df)
  # Subset to get the mmk table
  mmk_table <- df[15:length(df[, 1]), 1:8, drop = FALSE]
  mmk_table <- mmk_table[, -3] # Remove the 3 col. which is empty
  # Set the col. names for the mmk table
  mmk_col_name <- mmk_table[1, ]
  mmk_col_name[2] <- "X" # Replace the NA val.
  colnames(mmk_table) <- mmk_col_name
  mmk_table <- mmk_table[-1, ] # Remove the 1 row which is set as the col. names already
  # Get the calculated result for various params along with the description
  params_res <- df[4:11, 1:3, drop = FALSE]


  # Print the result
  cat("\n")
  cli::cli_alert_success("Solutions: ")
  cat("\n")
  print("Calculated Results:")
  print(params_res)
  cat("\n")
  print("MMK Table:")
  print(mmk_table)
  cat("\n")
  print("Resource in the Queue = Total People - People at the Service Point")
  cat("\n")
}

# Topic 5 (Control Charts)
# Main Menu List
menu_list_t5 <- c(
  "X-Chart [Average]",
  "P-Chart [Proportion]",
  "Back"
)

# Topic IV menu
topic_v <- function() {
  choice <- menu(menu_list_t5, title = "What do you need?")
  switch(choice,
    "1" = {
      x_chart()
      cat("\n")
      topic_v()
    },
    "2" = {
      p_chart()
      cat("\n")
      topic_v()
    },
    "3" = topic_select()
  )
}

x_chart <- function() {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x, row.names = 1)
  # Ask for the a2 value
  a2_val <- to_int(inp_split("Enter the A2 Value: "))
  # Get X double bar (population mean)
  col_means <- colMeans(df)
  x_db_bar <- mean(col_means)
  # Get R bar (average of population range)
  # Get min & max of each col.
  col_range <- data.frame(lapply(df, range))
  # Get the range of each
  col_range <- col_range[2, ] - col_range[1, ]
  # Calculate the mean of all of ranges
  r_bar <- mean(unlist(col_range))
  # Calculate the UCL (upper control limit)
  ucl <- x_db_bar + a2_val * r_bar
  # Calculate the LCL (lower control limit)
  lcl <- x_db_bar - a2_val * r_bar
  # Plot the data
  x_axis <- seq(1, length(colnames(df)), 1)
  y_axis <- unlist(col_means)
  x_chart_plot_data <- ggplot(as.data.frame(col_means), aes(x_axis, y_axis)) +
    geom_line(aes(group = 1, color = "Mean"), size = 2) +
    geom_hline(aes(group = 1, color = "X_DB_BAR", yintercept = x_db_bar), linetype = "dashed", size = 2) +
    geom_hline(aes(group = 1, color = "UCL", yintercept = ucl), linetype = "dashed", size = 2) +
    geom_hline(aes(group = 1, color = "LCL", yintercept = lcl), linetype = "dashed", size = 2) +
    # The colors of the value are mapped in alphabetical orders in terms of the legend names
    scale_colour_manual(name = "Type:", values = c("Red", "Black", "Green", "Yellow")) +
    ggtitle("X-Chart") +
    labs(y = NULL, x = NULL) +
    theme_bw()
  print(x_chart_plot_data)
  # Print the result
  cat("\n")
  cli::cli_alert_success("Results: ")
  cat("\n")
  print(paste("X_DOUBLE_BAR:", x_db_bar))
  cat("\n")
  print(paste("R_BAR:", r_bar))
  cat("\n")
  print(paste("UCL:", ucl))
  cat("\n")
  print(paste("LCL:", lcl))
  cat("\n")
}


p_chart <- function() {
  # Import the file
  x <- file_import(TRUE)
  # Ask for the n value (sample size)
  sample_szie <- to_int(inp_split("Enter the Sample Size (n): "))
  # Does the data need extra calculation?
  inc_count <- readline(prompt = "Is the Count of Inccorecct Occurrences Given? (y/n): ")
  if (identical(inc_count, "n")) {
    # Convert it to df
    df <- data.frame(x, row.names = 1)
    row_names <- rownames(df)
    df <- data.frame(lapply(df, as.numeric))
    # Re-assign the row names
    rownames(df) <- row_names
    # Ask for the incorrect measurement standard
    mes_std <- to_int(inp_split("Enter the Measurement Standard (e.g. 4, 0.6): "))
    # Ask for the incorrect measurement condition
    mes_cond <- readline(prompt = "Enter the Measurement Condition [<,>,<=,>=,=]: ")
    # Switch
    if (identical(mes_cond, "<")) {
      df <- ifelse(df < mes_std, 1, 0)
    } else if (identical(mes_cond, ">")) {
      df <- ifelse(df > mes_std, 1, 0)
    } else if (identical(mes_cond, "<=")) {
      df <- ifelse(df <= mes_std, 1, 0)
    } else if (identical(mes_cond, ">=")) {
      df <- ifelse(df >= mes_std, 1, 0)
    } else if (identical(mes_cond, "=")) {
      df <- ifelse(df == mes_std, 1, 0)
    }
    # Calculate the incorrect %
    incorrect_percentage <- colMeans(df)
    # Calculate P bar (avg. of all the incorrect percentages)
    p_bar <- mean(incorrect_percentage)
    # X-axis
    x_axis <- seq(1, length(colnames(df)), 1)
  } else {
    # Convert it to df
    df <- data.frame(x)
    # Calculate the incorrect %
    incorrect_percentage <- df[, 2] / sample_szie
    # Update the df
    df <- cbind(df, Percentage = incorrect_percentage)
    # Calculate P bar (avg. of all the incorrect percentages)
    p_bar <- colMeans(df[, "Percentage", drop = FALSE])
    # X-axis
    x_axis <- seq(1, length(rownames(df)), 1)
  }

  # Calculate UCL
  ucl <- p_bar + 3 * sqrt(p_bar * (1 - p_bar) / sample_szie)
  # Calculate LCL
  lcl <- p_bar - 3 * sqrt(p_bar * (1 - p_bar) / sample_szie)
  # Plot the data
  p_chart_plot_data <- ggplot(data.frame(incorrect_percentage), aes(x_axis, incorrect_percentage)) +
    geom_line(aes(group = 1, color = "Incorrect Percentage"), size = 2) +
    geom_hline(aes(group = 1, color = "P_BAR", yintercept = p_bar), linetype = "dashed", size = 2) +
    geom_hline(aes(group = 1, color = "UCL", yintercept = ucl), linetype = "dashed", size = 2) +
    geom_hline(aes(group = 1, color = "LCL", yintercept = lcl), linetype = "dashed", size = 2) +
    ggtitle("P-Chart") +
    labs(y = NULL, x = NULL) +
    theme_bw()

  # Is there a set limit for comparison?
  if_limit <- readline(prompt = "Is there a set limit for comparison? (y/n): ")
  if (identical(if_limit, "y")) {
    # Ask for the limit value
    limit_val <- to_int(inp_split("Enter the Limit Value (e.g. 0.2,0.5): "))
    # Update the plot obj
    p_chart_plot_data <- p_chart_plot_data +
      geom_hline(aes(group = 1, color = "Limit", yintercept = limit_val), linetype = "dashed", size = 2) +
      scale_colour_manual(name = "Type:", values = c("Black", "Red", "Purple", "Yellow", "Green"))
    print(p_chart_plot_data)
  } else {
    p_chart_plot_data <- p_chart_plot_data +
      # The colors of the value are mapped in alphabetical orders in terms of the legend names
      scale_colour_manual(name = "Type:", values = c("Black", "Red", "Yellow", "Green"))
    print(p_chart_plot_data)
  }
  # Print the result
  cat("\n")
  cli::cli_alert_success("Results: ")
  cat("\n")
  print(paste("P_BAR:", p_bar))
  cat("\n")
  print(paste("UCL:", ucl))
  cat("\n")
  print(paste("LCL:", lcl))
  cat("\n")
}

# Topic 6 (Inventory Management)
# Main Menu List
menu_list_t6 <- c(
  "Constant Demand",
  "Variable Demand",
  "Back"
)

# Topic VI menu
topic_vi <- function() {
  choice <- menu(menu_list_t6, title = "What do you need?")
  switch(choice,
    "1" = {
      inv_constant()
      cat("\n")
      topic_vi()
    },
    "2" = {
      inv_var()
      cat("\n")
      topic_vi()
    },
    "3" = topic_select()
  )
}

inv_constant() <- function() {
  # Import the file
  x <- file_import(TRUE)
  # Convert it to df
  df <- data.frame(x, row.names = 1)
  # Subset the df to get pure numerical value with the code being the row namespace
  pre.df <- df[, seq_len(length(colnames(df)))]
  df.1 <- data.frame(pre.df, row.names = 1)
  # Remove NAs
  df.1 <- df.1[, colSums(is.na(df.1)) == 0, drop = FALSE]
  # Calculate EQQ
  df.1["EQQ", ] <- sqrt(2 * df.1["L", ] * df.1["D", ] / (df.1["H", ] * df.1["C", ]))
  # Calculate the order per year
  df.1["DQ", ] <- df.1["D", ] / df.1["EQQ", ]
  # Calculate the cycle time
  df.1["CT", ] <- df.1["DOY", ] / df.1["DQ", ]
  # Calculate the management cost
  df.1["MC", ] <- df.1["L", ] * (df.1["D", ] / df.1["EQQ", ]) + df.1["H", ] * df.1["C", ] * (df.1["EQQ", ] / 2)
  # Calculate the reorder point
  df.1["RP", ] <- df.1["RLT", ] * df.1["DS", ]
  # Update the initial df with df.1
  df[, 2:length(colnames(df))] <- df.1
  # Ask if the lead time varies
  lt_v <- to_int(inp_split("Does the Lead Times Vary [Yes=1,No=0]: "))
  # Determine if the lead time varies
  if (identical(lt_v, 1)) {
    lt_v_d <- to_int(inp_split("How Many Days e.g 2 : "))
    lt_v_d <- seq(1, lt_v_d, 1)
    lt_v_p <- to_int(inp_split("Enter the Probability for Each Day in CSV (e.g. 0.1,0.2): "))
    s_rate <- to_int(inp_split("Enter the service Rate (e.g. 0.9): "))
    # The lead time probability table
    ltpt <- data.frame(t(rbind(Lead.Time = lt_v_d, Prob = lt_v_p)))
    ltpt <- data.frame(ltpt, Cumulative.Prob = 0, Demand.During.Lead.Time = 0)
    # Fill in the first prob.
    ltpt[, "Cumulative.Prob"][1] <- ltpt[, "Prob"][1]
    # Calculate the cumulative prob.
    for (i in seq_len(length(rownames(ltpt)))) {
      if (!is.na(ltpt[, "Cumulative.Prob"][i + 1])) {
        ltpt[, "Cumulative.Prob"][i + 1] <- ltpt[, "Cumulative.Prob"][i] + ltpt[, "Prob"][i + 1]
      }
    }
    # Calculate the demand during lead time
    ltpt[, "Demand.During.Lead.Time"] <- df.1["DS", ][1] * ltpt[, "Lead.Time"]
    # Calculate the average demand during the lead time
    avg_demand_dlt <- sum(ltpt[, "Prob"] * ltpt[, "Demand.During.Lead.Time"])
    # Find the index for the service level
    sr_rate_index <- which(ltpt[, "Cumulative.Prob"] == s_rate)
    # Calculate the safety stock
    sft_stock <- ltpt[, "Demand.During.Lead.Time"][sr_rate_index] - avg_demand_dlt

    # Print the result
    cat("\n")
    cli::cli_alert_success("Results: ")
    cat("\n")
    print(df)
    cat("\n")
    print(paste("Average Demand During Lead Time: ", avg_demand_dlt))
    cat("\n")
    print(paste("Safety Stock at Service Level", s_rate, ":", sft_stock))
  }

  # Print the result
  cat("\n")
  cli::cli_alert_success("Results: ")
  cat("\n")
  print(df)
  cat("\n")
}



# Main Menu Selection Function
topic_select <- function() {
  menu_list <- c(
    "Forecasting",
    "Probability Distribution",
    "Process Analysis",
    "Waiting Line",
    "Control Chart",
    "Inventory Management"
  )

  choice <- menu(menu_list, title = "Please Select A Topic:")
  # Menu Selection Function
  m_select <- function(topic) {
    switch(topic,
      "1" = topic_i(),
      "2" = topic_ii(),
      "3" = topic_iii(),
      "4" = topic_iv(),
      "5" = topic_v(),
      "6" = topic_vi()
    )
  }
  m_select(choice)
}
topic_select()