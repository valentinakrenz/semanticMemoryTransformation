### Valentina Krenz 2023

### analyses for the manuscript "Time-dependent memory transformation in hippocampus and neocortex is semantic in nature"
### by Valentina Krenz, Arjen Alink, Tobias Sommer, Benno Roozendaal & Lars Schwabe

# PACKAGES ####
# Tidyverse for data preparation
library(tidyverse)

# Importing data from Excel
library(readxl)
library(openxlsx)

# For descriptive statistics
library(psych)

# For ANOVA
library(afex)

# Post-hoc tests for ANOVAs and (generalized) linear mixed models
library(emmeans)

# For Cohen's d
library(lsr)

# For lmer/glmer -> mixed effects model
library(lme4)
library(optimx)
library(lmerTest) # show p_values in mixed effects model; masks lmer from lme4 and step from stats

# Packages for plotting
library(ggplot2) # For bar plots; masks %+%, alpha from psych
library(showtext)# to add fonds

# Necessary for SE within but masks rename (and other functions from dplyr)
library(Rmisc)

# For additional plotting features; masks mutate from plyr
library(ggpubr)

# Various other packages
library(MuMIn)
library(sjPlot) # plotting (generalized) linear mixed models; masks plot_grid and save_plot from cowplot
library(flextable) # masks border, font, rotate from ggpubr

options(scipen=999) # 999 if you don't want scientific notation for p-values

# FUNCTIONS ####
  sigma_res <- function(model=ANOVA) {
    residuals <- resid(model)
    sigma_value <- sd(residuals)
    return(sigma_value)
  }
  
  # effect size for unpaired tests # will give you the same d ass emmeans::eff_size()
  compute_effect_size <- function(emmeans=emmeans, model = ANOVA) {
    stat  <- as.data.frame(summary(emmeans)$contrasts)
    emmeans <- as.data.frame(summary(emmeans))
    
    # Determine the position of the "contrast" and "estimate" columns
    contrast_pos <- which(colnames(stat) == "contrast")
    estimate_pos <- which(colnames(stat) == "estimate")
    
    # Get any additional factor columns between "contrast" and "estimate"
    additional_factors <- colnames(stat)[(contrast_pos + 1):(estimate_pos - 1)]
    
    # Create unique identifiers based on "contrast" and any additional factors
    unique_identifiers <- apply(stat[, c("contrast", additional_factors)], 1, paste, collapse = " ")
    unique_identifiers <- unique(unique_identifiers)
    
    # Create a data frame to store the results
    result_df <- data.frame(
      contrast = character(),
      t = numeric(),
      df = numeric(),
      d = numeric(),
      lower_CI = numeric(),
      upper_CI = numeric(),
      p = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over the unique combinations of "contrast" and any additional factors
    for (id in unique_identifiers) {
      i <- which(apply(stat[, c("contrast", additional_factors)], 1, paste, collapse = " ") == id)[1]
      
      # Extract mean difference and standard error
      mean_difference <- stat$estimate[i]
      SE <- stat$SE[i]
      
      # Compute Cohen's d
      cohens_d <- mean_difference / sigma_res(model = ANOVA)
      
      # Compute the confidence intervals
      lower.CL <- cohens_d - 1.96 * SE / sigma_res(model = ANOVA) 
      upper.CL <- cohens_d + 1.96 * SE / sigma_res(model = ANOVA)
      
      # Get the contrast value, including any additional factors
      contrast_value <- paste(stat$contrast[i], sapply(additional_factors, function(x) paste("in", stat[[x]][i])))
      contrast_value <- paste(contrast_value, collapse = " ")
      
      # Add the results to the overall results data frame
      result_df <- rbind(result_df, data.frame(
        contrast = contrast_value,
        t = stat$t.ratio[i],
        df = stat$df[i],
        d = cohens_d,
        lower_CI = lower.CL,
        upper_CI = upper.CL,
        p = stat$p.value[i],
        stringsAsFactors = FALSE
      ))
    }
    
    return(result_df)
  }
  
  # effect size for interaction contrasts # includes adjustment of sigma
  compute_effect_size_interaction <- function(emmeans=emmeans, stat=stat, model=ANOVA, title=title) {
    stat  <- as.data.frame(stat)
    emmeans <- as.data.frame(summary(emmeans))
    
    # Get unique contrasts
    unique_contrasts <- unique(stat$contrast)
    print("Unique Contrasts:")
    print(unique_contrasts)
    
    # Create a data frame to store the results
    new_df <- data.frame(
      contrast = character(),
      t = numeric(),
      df = numeric(),
      d = numeric(),
      lower_CI = numeric(),
      upper_CI = numeric(),
      p = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over the unique contrast levels
    for (contrast in unique_contrasts) {
      print(paste("Processing Contrast:", contrast))
      # Filter data for the specific contrast
      contrast_stat <- stat[stat$contrast == contrast,]
      print("Contrast Statistics:")
      print(contrast_stat)
      
      # Extract mean difference and standard error (take the first row, as they are all the same)
      mean_difference <- contrast_stat$estimate[1]
      SE <- contrast_stat$SE[1]
      print("Mean Difference and Standard Error:")
      print(c(mean_difference, SE))
      
      # Compute Cohen's d
      cohens_d <- mean_difference / (sigma_res(model = model) * sqrt(2))
      print("Cohen's d:")
      print(cohens_d)
      
      # Compute the confidence intervals
      lower.CL <- cohens_d - 1.96 * SE / (sigma_res(model = model) * sqrt(2))
      upper.CL <- cohens_d + 1.96 * SE / (sigma_res(model = model) * sqrt(2))
      print("Confidence Intervals:")
      print(c(lower.CL, upper.CL))
      
      # Add the results to the overall results data frame
      new_df <- rbind(new_df, data.frame(
        contrast = contrast,
        t = contrast_stat$t.ratio[1],
        df = contrast_stat$df[1],
        d = cohens_d,
        lower_CI = lower.CL,
        upper_CI = upper.CL,
        p = contrast_stat$p.value[1],
        stringsAsFactors = FALSE))
        print("New Data Frame:")
        print(new_df)
    }
    print("Full Data Frame:")
    print(new_df)
    return(new_df)
  }

  # computes sigma based on difference between conditions
  compute_effect_size_paired <- function(emmeans = emmeans, data, group_variable = NULL, 
                                         within_variable, response_variable) {
    
    means <- as.data.frame(summary(emmeans$emmeans))
    stat  <- as.data.frame(summary(emmeans)$contrasts)
    
    result_df <- data.frame(
      contrast = character(),
      t = numeric(),
      df = numeric(),
      d = numeric(),
      lower_CI = numeric(),
      upper_CI = numeric(),
      p = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Determine if there are group variables and their levels
    if (is.null(group_variable)) {
      combinations <- data.frame(unique(stat$contrast))
      group_case <- 0
    } else if (length(group_variable) == 1) {
      group_variable1 <- group_variable[1]
      group_levels1 <- unique(means[[group_variable[1]]])
      combinations <- expand.grid(contrast = unique(stat$contrast), level1 = group_levels1)
      group_case <- 1
    } else if (length(group_variable) == 2) {
      group_variable1 <- group_variable[1]
      group_variable2 <- group_variable[2]
      group_levels1 <- unique(means[[group_variable[1]]])
      group_levels2 <- unique(means[[group_variable[2]]])
      combinations <- expand.grid(contrast = unique(stat$contrast), level1 = group_levels1, level2 = group_levels2)
      group_case <- 2
    }
   
    # Loop through the combinations
    for (comb_i in 1:nrow(combinations)) {
      combination <- combinations[comb_i,]
      
      print(paste("Processing combination:", paste(combination, collapse = ", ")))
      
      # Case with no group variables
      if (group_case == 0) {
        subset_data <- data
        contrast <- combination
        
        # Case with one group variable
      } else if (group_case == 1) {
        subset_data <- subset(data, data[[group_variable[1]]] == combination$level1)
        contrast <- combination$contrast
        
        # Case with two group variables
      } else if (group_case == 2) {
        print(paste("Subsetting by contrast == ", combination$contrast, "and", group_variable1, "==", combination$level1, "and", group_variable2, "==", combination$level2))
        subset_data <- subset(data, data[[group_variable[1]]] == combination$level1 & data[[group_variable[2]]] == combination$level2)
        contrast <- combination$contrast
      }
      
      contrast <- as.character(contrast)
      within_values <- unlist(strsplit(contrast, ' - '))
      
      #within_values1 <- subset_data[subset_data[[within_variable]] == within_values[1], response_variable]
      #within_values2 <- subset_data[subset_data[[within_variable]] == within_values[2], response_variable]
      
      within_values1 <- subset_data[subset_data[[within_variable]] == within_values[1], response_variable]
      within_values2 <- subset_data[subset_data[[within_variable]] == within_values[2], response_variable]
      
      differences <- within_values1 - within_values2
      sigma_difference <- sd(differences)
      
      # Determine the correct row based on the group_case
      if (group_case == 0) {
        correct_row <- stat[grepl(contrast, stat$contrast), ]
      } else if (group_case == 1) {
        correct_row <- subset(stat, contrast == combination$contrast & stat[[group_variable1]] == combination$level1)
      } else if (group_case == 2) {
        correct_row <- subset(stat, contrast == combination$contrast & stat[[group_variable1]] == combination$level1
                              & stat[[group_variable2]] == combination$level2)
      }
      print(correct_row)
      mean_difference <- correct_row$estimate
      SE <- correct_row$'SE'
      
      cohens_d <- mean_difference / sigma_difference
      lower.CL <- cohens_d - 1.96 * SE / sigma_difference
      upper.CL <- cohens_d + 1.96 * SE / sigma_difference
      
      print(cohens_d)
      print(lower.CL)
      print(upper.CL)
      
      # Initialize group_label as an empty string
      group_label <- ""
      
      # Check if 'level1' exists in 'combination' and add it to the label if it does
      if (group_case == 1 || group_case == 2) {
        if (!is.null(combination$level1)) {
          group_label <- paste("in", combination$level1)
        }
      }
      
      # Check if 'level2' exists in 'combination' and add it to the label if it does
      if (group_case == 2) {
        if (!is.null(combination$level2)) {
          group_label <- paste(group_label, "and", combination$level2)
        }
      }

      group_df <- data.frame(
        contrast = paste(contrast, group_label),
        t = correct_row$t.ratio,
        df = correct_row$df,
        d = cohens_d,
        lower_CI = lower.CL,
        upper_CI = upper.CL,
        p = correct_row$p.value,
        stringsAsFactors = FALSE
      )
      
      result_df <- rbind(result_df, group_df)
    }

    return(result_df)
  }
  
  # Apply rounding rounding specifically for the p-value column
  round_p <- function(table_df, p_col = ncol(table_df)) {
    table_df[, p_col] <- sapply(table_df[, p_col], function(x) {
      if (x >= 0.0005) {
        result <- round(x, 3)
      } else if (x >= 0.00005) {
        result <- round(x, 4)
      } else if (x < 9e-99) {
        result <- "< 9e-99"
      } else {
        result <- result <- format(x, digits = 1, scientific = TRUE) # Use scientific notation
      }
      result
    })
    return(table_df)
  }
  
  # round 2 spaces
  round_two_spaces <- function(table_df=table_df, start_col = 2, end_col = ncol(table_df)-1){
    for (j in start_col:end_col) { # Exclude the variable, d, and p-value columns
      table_df[, j] <- sapply(table_df[, j], function(x) {
        if (!is.nan(x)) { # Check if the value is not NaN
          abs_x <- abs(x)
          if (abs_x >= 0.005) {
            result <- round(x, 2)
          } else if (abs_x >= 0.0005) {
            result <- round(x, 3)
          } else if (abs_x == 0) {
            result <- "0.00"
          } else if (abs_x < 0.0005) {
            result <- result <- format(x, digits = 1, scientific = TRUE) # Use scientific notation
          }
        } else { # If the value is NaN, retain it as is
          result <- x
        }
        result # return the result for each element inside the sapply function
      })
    }
    return(table_df) # return the entire data frame outside the loop
  }
  
  # get ci for partial eta squared 
  compute_eta_squared_CI <- function(ANOVA, row_index){
    # Given F value, numerator df (effect df) and denominator df (error df)
    F_val <- ANOVA$anova_table$F[row_index]
    df_num <- ANOVA$anova_table$'num Df'[row_index]
    df_error <- ANOVA$anova_table$'den Df'[row_index]
    
    # Calculate non-central parameter
    lambda <- F_val * df_num
    
    # Calculate critical F values for 95% CI
    alpha <- 0.05
    F_lower <- qf(alpha / 2, df_num, df_error, lambda, lower.tail=TRUE)
    F_upper <- qf(1 - alpha / 2, df_num, df_error, lambda)
    
    # Convert to partial eta squared
    eta_lower <- df_num * F_lower / (df_num * F_lower + df_error)
    eta_upper <- df_num * F_upper / (df_num * F_upper + df_error)
    
    return(list(lower = eta_lower, upper = eta_upper))
  }
  
  
  # bring ANOVA results to a nice table and get ci
  get_ANOVA_results <- function(ANOVA=ANOVA) {
    # get results in correct format 
    anova_table <- ANOVA$anova_table
    table_df <- as.data.frame(ANOVA$anova_table)
    variable_names <- ANOVA$Anova$terms[-1]
    table_df <- cbind(Variable = variable_names, table_df)
    rownames(table_df) <- NULL
    # Initialize new columns to store the lower and upper limits of the confidence intervals
    table_df$LL_CI <- NA
    table_df$UL_CI <- NA
    alpha = 0.05
    # Loop through each variable and compute the 95% confidence interval
    for (i in 1:nrow(table_df)) {
      ci <- compute_eta_squared_CI(ANOVA, row_index=i)
      table_df$LL_CI[i] <- ci$lower
      table_df$UL_CI[i] <- ci$upper
    }
    
    # Reorganize columns
    table_df <- table_df[, c("Variable", "num Df", "den Df", "F", "LL_CI", "UL_CI", "pes", "Pr(>F)")]
    
    # Assuming these functions are already defined in your script
    table_df <- round_p(table_df = table_df)
    table_df <- round_two_spaces(table_df = table_df, start_col = 2)
    
    # add multiplication sign to interaction variable
    table_df$Variable <- gsub(":", " × ", table_df$Variable)
    # Concatenate the values from the "num Df" and "den Df" columns into the "F" column
    table_df$F <- paste(table_df$F, " (", table_df$`num Df`, ", ", table_df$`den Df`, ")", sep = "")
    # Concatenate the LL_CI and UL_CI columns to create the "95% CI" column
    table_df$`95% CI` <- paste("[", table_df$LL_CI, ", ", table_df$UL_CI, "]", sep = "")
    # Remove the "num Df" and "den Df" columns as they're now part of the "F" column
    table_df <- table_df[, -c(2, 3)]
    # Select the columns you want to keep and reorder them
    table_df <- table_df[, c("Variable","F", "Pr(>F)","pes", "95% CI")]
    # Update column names if necessary
    colnames(table_df) <- c("variable","F", "p","pes", "95% CI")
    
    return(table_df)
  }
  # save in text file
  save_ANOVA_text <- function(table_df=table_df, title, file_name = "output", show_file = TRUE) {
    # Add the .txt extension to the file name
    file_name <- paste0(file_name, ".txt")
    # Open the file for appending
    file_conn <- file(file_name, "a")
    # Write the header
    cat("\n", title, "\n", file = file_conn)
    
    # Iterate over the rows of table_df
    for (i in 1:nrow(table_df)) {
      # Extract the relevant elements
      variable <- table_df$variable[i]
      
      # Add "main effect" prefix if there is no multiplication sign in the variable name
      if (grepl("×", variable) == FALSE) {
        variable <- paste("main effect", variable)
      }
      
      F_values <- gsub(" ", "", sub(".*\\(", "", sub("\\).*", "", table_df$F[i]))) # Extract the df entries inside brackets
      F_stat <- sub(" \\(.*", "", table_df$F[i]) # Extract the F value outside brackets
      
      # Construct the formatted string
      formatted_string <- paste(variable, ": F (", F_values, ") = ", F_stat, ", p = ", 
                                table_df$p[i], ", pes = ", table_df$pes[i], ", ",
                                "95% Confidence Interval: ", table_df$'95% CI'[i], sep = "")
      
      # Write the formatted string to the file
      cat(formatted_string, "\n", file = file_conn)
    }
    
    # Close the file connection
    close(file_conn)
    
    # Open the text file with the system's default text editor if show_file is TRUE
    if (show_file) {
      file.show(file_name)
    }
  }
  
  # save post hoc t-test statistic in text file
  save_postHoc_t_text <- function(table_df, start_col = 2, header_row, file_name = "output", show_file = TRUE, round = TRUE) {

    # Add the .txt extension to the file name
    file_name <- paste0(file_name, ".txt")
    # Open the file for appending
    file_conn <- file(file_name, "a")
    # Write the header
    cat("\n\n", header_row, "\n", file = file_conn)
    
    prev_title <- NULL
    
    if (round){
    # Round the data as specified
    table_df <- round_two_spaces(table_df, start_col = start_col, end_col = ncol(table_df)-1)
    table_df <- round_p(table_df=table_df, p_col=ncol(table_df))
    }
    
    # Iterate through the rows of the data frame
    for (i in 1:nrow(table_df)) {
      title <- table_df$title[i]
      contrast <- table_df$contrast[i]
      t_val <- table_df$t[i]
      df_val <- table_df$df[i]
      p_val <- table_df$p[i]
      d_val <- table_df$d[i]
      lower_CI <- table_df$lower_CI[i]
      upper_CI <- table_df$upper_CI[i]
      
      # If the title has changed since the previous row, print the title
      if (!is.null(prev_title) && prev_title != title) {
        cat("\n", file = file_conn) # Add a space between each type of title
      }
      if (is.null(prev_title) || prev_title != title) {
        cat("Title:", title, "\n\n", file = file_conn) # Print the title
      }
      
      # Construct the strings
      title_string <- paste("Contrast:", contrast, "\n")
      t_string <- paste("t(", df_val, ") = ", t_val, ", ", sep = "")
      p_string <- paste("p = ", p_val, ", ", sep = "")
      d_string <- paste("d = ", d_val, ", ", sep = "")
      CI_string <- paste("95% Confidence Interval = [", lower_CI, ", ", upper_CI, "]\n", sep = "")
      
      # Concatenate the strings and print to the file
      result_string <- paste(title_string, t_string, p_string, d_string, CI_string)
      cat(result_string, file = file_conn)
      
      # Update the previous title variable
      prev_title <- title
    }
    
    # Close the file connection
    close(file_conn)
    
    # Open the text file with the system's default text editor if show_file is TRUE
    if (show_file) {
      file.show(file_name)
    }
  }
  
  # bring manually computed t-tests and effect sized into table 
  get_manual_contrasts <- function(ttest=ttest, eff=eff, title=title) {
    
    t <- ttest$statistic
    df <- ttest$parameter
    p <- ttest$p.value
    cohen_d <- as.data.frame(eff$cohen.d)
    lower_CI <- cohen_d["lower"]
    upper_CI <- cohen_d["upper"]
    effect_size <- cohen_d["effect"]
    
    # Creating the dataframe
    result_df <- data.frame(
      contrast = title,
      t = t,
      df = df,
      d = effect_size[1,],
      lower_CI = lower_CI[1,],
      upper_CI = upper_CI[1,],
      p = p
    )
    result_df <- round_two_spaces(table_df = result_df, start_col = 2, end_col = ncol(result_df)-1)
    result_df <- round_p(table_df = result_df)
    # Printing the results
    return(result_df)
  }
  
  get_gLMM_results <- function(gLMM = gLMM, title){
    
    summary <- summary(gLMM)
    model_header <- paste0("parameters for generalized linear mixed model")
    
    conf = confint(gLMM, method="Wald")
    # Find the row index for '(Intercept)'
    intercept_row <- which(rownames(conf) == "(Intercept)")
    # Keep only the rows from '(Intercept)' on
    conf <- conf[(intercept_row):nrow(conf), ]
    #rename conf cols
    colnames(conf) <- c("lower_CI", "upper_CI")
    
    # Extract coefficients
    coefficients <- summary$coefficients
    
    # Combine the two into a new data frame
    coefficients <- cbind(coefficients, conf)
    
    # Create a data frame
    fixed <- data.frame(
      predictor = rownames(coefficients), beta = coefficients[, "Estimate"],
      z = coefficients[, "z value"], lower_CI = coefficients[, "lower_CI"], upper_CI = coefficients[, "upper_CI"],
      p = coefficients[, "Pr(>|z|)"]
    )
    
    # Compute standard errorsstd_error <- coefficients[, "Std. Error"]
    fixed$predictor <- gsub("\\(Intercept\\)", "intercept", fixed$predictor)
    fixed$predictor <- gsub("delay28d", "delay", fixed$predictor)
    fixed$predictor <- gsub("emotionnegative", "emotion", fixed$predictor)
    fixed$predictor <- gsub("percRatingGroupMeanCent", "perceptual rel.", fixed$predictor)
    fixed$predictor <- gsub("semRatingGroupMeanCent", "semantic rel.", fixed$predictor)
    fixed$predictor <- gsub(":", " × ", fixed$predictor)
    
    fixed <- round_two_spaces(table_df = fixed, start_col = 2, end_col = ncol(fixed)-1)
    fixed <- round_p(table_df = fixed)
    
    fixed$`95% CI` <- paste(fixed$lower_CI, ", ", fixed$upper_CI, sep = "")
    
    fixed <- as.data.frame(fixed[, c("predictor", "z", "p", "beta",  "95% CI")])
    rownames(fixed) <- NULL
    # Extract the standard deviations of the random effects
    random <- as.data.frame(VarCorr(gLMM))
    # Delete var2
    random$var2 <- NULL
    ngrps <- as.data.frame(summary[["ngrps"]])
    colnames(ngrps) <- "n"
    random$n <- ngrps[,1]
    # Rename values in the grp_var1 column
    random$grp <- gsub("stimulusTypeNum", "stimulus (intercept)", random$grp)
    random$grp <- gsub("set", "stimulus set (intercept)", random$grp)
    random$grp <- gsub("Name", "participant (intercept)", random$grp)
    # Find the row index where the 'grp' column has the value "participant"
    participant_row <- which(random$grp == "participant (intercept)")
    # Rearrange the rows so that the participant row comes first
    random <- rbind(random[participant_row, ], random[-participant_row, ])
    # Rename columns vcov and sdcor
    names(random)[names(random) == "vcov"] <- "variance"
    names(random)[names(random) == "sdcor"] <- "SD"
    # Select the desired columns in the final data frame
    random <- as.data.frame(random[, c("grp", "variance", "SD","n")])
    random = round_two_spaces(table_df = random, start_col = 2, end_col=ncol(random)-1)
    colnames(random)[1] <- "random effects"
    
    # Calculate R-squared values
    r2_values <- r.squaredGLMM(gLMM)
    # Create a data frame with the results
    r2 <- data.frame(
      marginal = r2_values["theoretical", "R2m"],
      conditional = r2_values["theoretical", "R2c"],
      row.names = "R2"
    )
    
    r2 <- as.data.frame(round_two_spaces(table_df = r2, start_col = 1, end_col = ncol(r2)))
    # Add a new column with the name "R2" and NA as its value
    r2 <- data.frame(R2 = NA, r2)
    # write string for note
    r2_note <- paste0("marginal R² / conditional R²: ", paste(r2$marginal, "/", r2$conditional))
    
    # Define the number of rows for each section
    n_fixed_rows <- nrow(fixed) + 1 # 2 extra for the title and header row
    n_random_rows <- nrow(random) + 1
    
    # Create an empty dataframe with the total number of rows and columns equal to the largest dataframe
    combined_df <- data.frame(matrix(ncol = ncol(fixed), nrow = n_fixed_rows + n_random_rows + 1)) # 3 extra lines for empty line, aic/bic, r2 
    # Add "Fixed Effects" title in the first column
    combined_df[1, 1] <- "fixed effects"
    # Add fixed data
    combined_df[1, 2:ncol(fixed)] <- colnames(fixed)[2:ncol(fixed)]
    combined_df[1, 4] <- "\u03B2" # unicode for lower case beta
    combined_df[2:(n_fixed_rows), ] <- as.data.frame(fixed)

    combined_df[n_fixed_rows + 1, 1:ncol(random)] <- colnames(random)
    combined_df[(n_fixed_rows + 2):(n_fixed_rows + n_random_rows), 1:ncol(random)] <- as.data.frame(random)
    
    combined_df <- as.data.frame(combined_df)
    # Create a new row with the desired header
    header_row_df <- data.frame(matrix(ncol = ncol(combined_df), nrow = 1))
    header_row_df[1, 1] <- model_header
    # Combine the header row with the existing data frame
    combined_df <- bind_rows(header_row_df, combined_df)
    combined_df[nrow(combined_df),1] = r2_note
    
    result <- combined_df
    
return(result)
  }   
  
  create_gLMM_table <- function(result = result, file_name, save = TRUE, show = TRUE) {
    
    table_df <- result
    table_df <- table_df[-1,]
    model_header <- "parameters for generalized linear mixed model"
    
    # find rows of fixed and random effetc title
    row_predictor <- which(table_df$X1 == "fixed effects")
    row_random_effects <- which(table_df$X1 == "random effects")
    # Calculating the number of rows between the two rows
    number_predictors <- row_random_effects - row_predictor - 1 
    number_predictors
    # Counting the number of rows after the "random effects" row
    number_random_effects <- nrow(table_df) - row_random_effects  # 3 extra lines
    number_random_effects
    
    ft <- flextable(table_df) %>%
      delete_part(part = "header") %>%
      add_header_lines(model_header) %>%
      align(align = "center", part = "header") %>%
      align(align = "center", part = "all", j = 2:ncol(table_df)) %>%
      hline(i=row_predictor, j=2:ncol(table_df), fp_border_default(color="black",width=1)) %>%
      hline(i=row_random_effects, j=2:(ncol(table_df)-1), fp_border_default(color="black",width=1)) %>%
      hline_top(j=1:ncol(table_df), border = fp_border_default
                (color="black",width=1), part = "header") %>%
      hline_top(j=1:ncol(table_df), border = fp_border_default
                (color="black",width=1)) %>%
      hline_bottom(j=1:ncol(table_df), border = 
                     fp_border_default(color="black",width=1)) %>%
      align(align = "center", part = "all", j = 2:ncol(table_df)) %>%
      bold(i=c(row_predictor,row_random_effects), j=1) %>%
      bold(part = "header") %>%
      italic(i=1, j=c(2,3,4)) %>%
      italic(i=row_random_effects, j=4) %>%
      padding(i=c((row_predictor+1):(number_predictors+1)), j=1, padding.left=20) %>%
      padding(i=c((row_random_effects+1):(row_random_effects + number_random_effects-1)), j=1, padding.left=20) %>%
      line_spacing(space = 0.5, part = "body") %>%
      merge_at(i=nrow(table_df), j=1:ncol(table_df))%>%
      add_footer_lines(as_paragraph("Source data are provided as Source Data file.")) %>%
      padding(i=c(row_predictor, row_random_effects), padding.bottom=2) %>%
      fontsize(size=10, part="all") %>%
      autofit()
    ft
    
    if(save){
      table_path <- paste0(file_path, '/result_tables')
      
      if (!dir.exists(table_path)) {
        dir.create(table_path)
        cat("Directory created:", table_path, "\n")
      } else {
        cat("Directory already exists:", table_path, "\n")
      }
      
      full_file_path <- normalizePath(paste0(table_path,"/",file_name, ".docx"))
      save_as_docx(ft, path = full_file_path)
      
      # Conditionally open the created file
      if (show) {
        shell.exec(full_file_path)
      }
    }
    return(ft)
    
  }
  
  # save gLMM report to text file
  save_gLMM_text <- function(table_df=table_df, file_name = "output", title=header_row, show_file = TRUE) {
    # Add the .txt extension to the file name
    file_name <- paste0(file_name, ".txt")
    # Open the file for appending
    file_conn <- file(file_name, "a")
    # Write the header
    #title <- table_df[1, 1]
    title=header_row
    cat("\n", title, "\n", file = file_conn)
    
    table_df=table_df[2:6,]
    colnames(table_df)=NULL
    # Set the column names to the values in the first row
    colnames(table_df) <- as.character(table_df[1, ])
    # Remove the first row
    table_df <- table_df[-1, ]
    
    # Iterate over the rows of table_df
    for (i in 2:nrow(table_df)) {
      # Extract the relevant elements
      variable <- table_df$`fixed effects`[i]
      
      # Add "main effect" prefix if there is no multiplication sign in the variable name and it's not the intercept
      if (grepl("×", variable) == FALSE) {
        variable <- paste("main effect", variable)
      }
      
      # Construct the formatted string
      formatted_string <- paste0(variable, ": z = ", table_df$z[i],", p = ", 
                                table_df$p[i], ", β = ", table_df$'β'[i],
                                ", 95% Confidence Interval: [", table_df$'95% CI'[i],"]")
      
      # Write the formatted string to the file
      cat(formatted_string, "\n", file = file_conn)
    }
    
    # Close the file connection
    close(file_conn)
    
    # Open the text file with the system's default text editor if show_file is TRUE
    if (show_file) {
      file.show(file_name)
    }
  }
  
  # order results of z-test into table and compute d and ci
  get_z_results <- function(emmeans=emmeans, gLMM=gLMM, title=title, edf=Inf) {
    stat <- as.data.frame(emmeans$contrasts)
    eff <- eff_size(emmeans, sigma = sigma(gLMM), edf = edf)
    eff_df <- as.data.frame(eff)
    
    # Determine the position of the "contrast" and "estimate" columns
    contrast_pos <- which(colnames(stat) == "contrast")
    estimate_pos <- which(colnames(stat) == "estimate")
    
    # Get any additional factor columns between "contrast" and "estimate"
    additional_factors <- colnames(stat)[(contrast_pos + 1):(estimate_pos - 1)]
    # Create unique identifiers based on "contrast" and any additional factors
    unique_identifiers <- apply(stat[, c("contrast", additional_factors)], 1, paste, collapse = " ")
    unique_identifiers <- unique(unique_identifiers)
    
    # Extract relevant information from the 'stat' and 'eff_df' data frames
    result_df <- data.frame(
      title=character(),
      contrast = character(),
      z = numeric(),
      d = numeric(),
      lower_CI = numeric(),
      upper_CI = numeric(),
      p = numeric()
    )
    
    for (unique_identifier in unique_identifiers) {
      
      # Create a logical index for rows that match the unique_identifier
      row_index <- apply(stat[, c("contrast", additional_factors)], 1, paste, collapse = " ") == unique_identifier
      
      # Subset stat and eff_df based on the row_index
      subset_stat <- stat[row_index,]
      subset_eff_df <- eff_df[row_index,]
      

    # Extract relevant information from the 'stat' and 'eff_df' data frames
    new_df <- data.frame(
      contrast = unique_identifier,
      z = subset_stat$z.ratio,
      d = subset_eff_df$effect.size,
      lower_CI = subset_eff_df$asymp.LCL,
      upper_CI = subset_eff_df$asymp.UCL,
      p = subset_stat$p.value
    )
    
    new_df <- round_two_spaces(table_df=new_df)
    new_df <- round_p(table_df = new_df, p_col = ncol(new_df))
    # Bind the title column to the beginning of new_df
    title_column <-data.frame(title = rep(title, nrow(new_df)))
    new_df <- cbind(title_column, new_df)
    
    result_df = rbind(result_df, new_df)
    }
    
    return(result_df)
  }
  
  # save z test report in text file
  save_z_text <- function(table_df=table_df,file_name = "output", show_file = TRUE) {
    # Add the .txt extension to the file name
    file_name <- paste0(file_name, ".txt")
    # Open the file for appending
    file_conn <- file(file_name, "a")
    # Write the header
    title <- table_df[i, 1]
    cat("\n post hoc z-test \n", file = file_conn)
    #table_df=table_df[2:6,]
    #colnames(table_df)=NULL
    # Remove the first row
    #table_df <- table_df[-1, ]
    i=1
    # Iterate over the rows of table_df
    for (i in 1:nrow(table_df)) {
      # Extract the relevant elements
      title <- table_df[i, 1]
      cat("\n", title, "\n", file = file_conn)
      
      variable <- table_df$contrast[i]
      
      # Construct the formatted string
      formatted_string <- paste(variable, ": z = ", table_df$z[i],", p = ", 
                                table_df$p[i], ", d = ", table_df$d[i],
                                ", 95% Confidence Interval: [", table_df$lower_CI[i], ", ", table_df$upper_CI[i], "]", sep = "")
      
      # Write the formatted string to the file
      cat(formatted_string, "\n", file = file_conn)
    }
    
    # Close the file connection
    close(file_conn)
    
    # Open the text file with the system's default text editor if show_file is TRUE
    if (show_file) {
      file.show(file_name)
    }
  }
  
  # effect size for unpaired tests # will give you the same d as emmeans::eff_size()
  compute_effect_size_z_interaction <- function(stat=stat, model = gLMM, title=title) {
    stat = as.data.frame(stat)

    # Determine the position of the "contrast" and "estimate" columns
    contrast_pos <- which(colnames(stat) == "contrast")
    estimate_pos <- which(colnames(stat) == "estimate")
    
    # Check if there are any additional factor columns between "contrast" and "estimate"
    if (contrast_pos + 1 == estimate_pos || contrast_pos == estimate_pos - 1) {
      unique_identifiers <- stat$contrast
    } else {
      # Get any additional factor columns between "contrast" and "estimate"
      additional_factors <- colnames(stat)[(contrast_pos + 1):(estimate_pos - 1)]
      # Create unique identifiers based on "contrast" and any additional factors
      unique_identifiers <- apply(stat[, c("contrast", additional_factors)], 1, paste, collapse = " ")
      unique_identifiers <- unique(unique_identifiers)
    }
    
    
    # Create a data frame to store the results
    result_df <- data.frame(
      title = character(),
      contrast = character(),
      z = numeric(),
      d = numeric(),
      lower_CI = numeric(),
      upper_CI = numeric(),
      p = numeric(),
      stringsAsFactors = FALSE
    )
    
   # Iterate over the unique combinations of "contrast" and any additional factors
      for (id in unique_identifiers) {
        if (contrast_pos + 1 == estimate_pos || contrast_pos == estimate_pos - 1) {
          i <- which(stat$contrast == id)[1]
          contrast_value = stat$contrast[i]
        } else {
          i <- which(apply(stat[, c("contrast", additional_factors)], 1, paste, collapse = " ") == id)[1]
          # Get the contrast value, including any additional factors
          contrast_value <- paste(stat$contrast[i], sapply(additional_factors, function(x) paste("in", stat[[x]][i])))
          contrast_value <- paste(contrast_value, collapse = " ")
        }

      # Extract mean difference and standard error
      mean_difference <- stat$estimate[i]
      SE <- stat$SE[i]
      
      # Compute Cohen's d
      cohens_d <- mean_difference / sigma(gLMM)
      
      # Compute the confidence intervals
      lower.CL <- cohens_d - 1.96 * SE / sigma(gLMM) 
      upper.CL <- cohens_d + 1.96 * SE / sigma(gLMM)

      
      # Add the results to the overall results data frame
      result_df <- rbind(result_df, data.frame(
        title = title,
        contrast = contrast_value,
        z = stat$z.ratio[i],
        d = cohens_d,
        lower_CI = lower.CL,
        upper_CI = upper.CL,
        p = stat$p.value[i],
        stringsAsFactors = FALSE
      ))
    }
    result_df <- round_two_spaces(table_df = result_df, start_col = 3)
    result_df <- round_p(table_df = result_df, p_col = ncol(result_df))
    return(result_df)
  }
  
  get_LMM_results <- function(LMM = LMM, Bonf = FALSE){
    
    summary <- summary(LMM)

    shorter_model_header <- paste("parameters for linear mixed model")
    
    # Extract coefficients
    coefficients <- summary$coefficients
    
    conf = confint(LMM, method="Wald")
    # Find the row index for '(Intercept)'
    intercept_row <- which(rownames(conf) == "(Intercept)")
    # Keep only the rows from '(Intercept)' on
    conf <- conf[(intercept_row):nrow(conf), ]
    #rename conf cols
    colnames(conf) <- c("lower_CI", "upper_CI")
    
    # Combine the two into a new data frame
    coefficients <- cbind(coefficients, conf)
    
    # Create a data frame
    fixed <- data.frame(
      predictor = rownames(coefficients), beta = coefficients[, "Estimate"],
      t = coefficients[, "t value"], df = coefficients[, "df"],  
      lower_CI = coefficients[, "lower_CI"], upper_CI = coefficients[, "upper_CI"],
      p = coefficients[, "Pr(>|t|)"]
    )
    
    # Compute standard errorsstd_error <- coefficients[, "Std. Error"]
    fixed$predictor <- gsub("\\(Intercept\\)", "intercept", fixed$predictor)
    fixed$predictor <- gsub("longaxisposterior", "long axis", fixed$predictor)
    fixed$predictor <- gsub("delay28d", "delay", fixed$predictor)
    fixed$predictor <- gsub("emotionnegative", "emotion", fixed$predictor)
    fixed$predictor <- gsub(":", " × ", fixed$predictor)
    
    fixed <- round_two_spaces(table_df = fixed, start_col = 2, end_col = ncol(fixed)-1)
    if (Bonf){fixed$p = fixed$p*2}
    fixed <- round_p(table_df = fixed)
    
    fixed$`95% CI` <- paste(fixed$lower_CI, ", ", fixed$upper_CI, sep = "")
    
    fixed$`t (df)` <- paste0(fixed$t, " (", fixed$df, ")")
    fixed <- as.data.frame(fixed[, c("predictor", "t (df)", "p","beta","95% CI")])
    rownames(fixed) <- NULL
    # Extract the standard deviations of the random effects
    random <- as.data.frame(VarCorr(LMM))
    if ("Residual" %in% random$grp) {
      random <- random[random$grp != "Residual", ]
    }
    # Delete var2
    random$var2 <- NULL
    ngrps <- as.data.frame(summary[["ngrps"]])
    colnames(ngrps) <- "n"
    random$n <- ngrps[,1]
    # Rename values in the grp_var1 column
    random$grp <- gsub("stimulusTypeNum", "stimulus (intercept)", random$grp)
    random$grp <- gsub("set", "stimulus set (intercept)", random$grp)
    random$grp <- gsub("Name", "participant (intercept)", random$grp)
    # Find the row index where the 'grp' column has the value "participant"
    participant_row <- which(random$grp == "participant (intercept)")
    # Rearrange the rows so that the participant row comes first
    random <- rbind(random[participant_row, ], random[-participant_row, ])
    # Rename columns vcov and sdcor
    names(random)[names(random) == "vcov"] <- "variance"
    names(random)[names(random) == "sdcor"] <- "SD"
    # Select the desired columns in the final data frame
    random <- as.data.frame(random[, c("grp", "variance", "SD","n")])
    random = round_two_spaces(table_df = random, start_col = 2, end_col=ncol(random)-1)
    colnames(random)[1] <- "random effects"
    
    # Calculate R-squared values
    r2_values <- r.squaredGLMM(gLMM)
    # Create a data frame with the results
    r2 <- data.frame(
      marginal = r2_values["theoretical", "R2m"],
      conditional = r2_values["theoretical", "R2c"],
      row.names = "R2"
    )
    
    r2 <- as.data.frame(round_two_spaces(table_df = r2, start_col = 1, end_col = ncol(r2)))
    # Add a new column with the name "R2" and NA as its value
    r2 <- data.frame(R2 = NA, r2)
    # write string for note
    r2_note <- paste0("marginal R² / conditional R²: ", paste(r2$marginal, "/", r2$conditional))
    
    # Define the number of rows for each section
    n_fixed_rows <- nrow(fixed) + 1 # 2 extra for the title and header row
    n_random_rows <- nrow(random) + 1
    
    # Create an empty dataframe with the total number of rows and columns equal to the largest dataframe
    combined_df <- data.frame(matrix(ncol = ncol(fixed), nrow = n_fixed_rows + n_random_rows +1)) # 1 extra line for r2
    # Add "Fixed Effects" title in the first column
    combined_df[1, 1] <- "fixed effects"
    # Add fixed data
    combined_df[1, 2:ncol(fixed)] <- colnames(fixed)[2:ncol(fixed)]
    combined_df[1, 4] <- "\u03B2" # unicode for lower case beta
    combined_df[2:(n_fixed_rows), ] <- as.data.frame(fixed)
    
    combined_df[n_fixed_rows + 1, 1:ncol(random)] <- colnames(random)
    combined_df[(n_fixed_rows + 2):(n_fixed_rows + n_random_rows), 1:ncol(random)] <- as.data.frame(random)
    
    combined_df <- as.data.frame(combined_df)
    # Create a new row with the desired header
    header_row_df <- data.frame(matrix(ncol = ncol(combined_df), nrow = 1))
    header_row_df[1, 1] <- shorter_model_header
    # Combine the header row with the existing data frame
    combined_df <- bind_rows(header_row_df, combined_df)
    combined_df[nrow(combined_df),1] = r2_note
    if (Bonf){
      combined_df[2,3] = "p cor"
    }
    
    result <- list(
      combined_df = combined_df,
      shorter_model_header = shorter_model_header, model_header = model_header
    ) 
    
    return(result)
  }   
  
  create_LMM_table <- function(result = result, file_name, save = TRUE, show = TRUE) {
    
    table_df <- result$combined_df
    
    # find rows of fixed and random effetc title
    row_predictor <- which(table_df$X1 == "fixed effects")
    row_random_effects <- which(table_df$X1 == "random effects")
    # Calculating the number of rows between the two rows
    number_predictors <- row_random_effects - row_predictor - 1 
    number_predictors
    # Counting the number of rows after the "random effects" row
    number_random_effects <- nrow(table_df) - row_random_effects - 1  # 3 extra lines
    number_random_effects
    
    ft <- flextable(table_df) %>%
      delete_part(part = "header") %>%
      merge_at(i=1, j=1:ncol(table_df))%>%
      bold(i = 1) %>%
      align(align = "center", i =1) %>%
      align(align = "center", part = "all", j = 2:ncol(table_df)) %>%
      hline(i=row_predictor, j=2:ncol(table_df), fp_border_default(color="black",width=1)) %>%
      hline(i=row_random_effects, j=2:(ncol(table_df)-1), fp_border_default(color="black",width=1)) %>%
      hline_top(j=1:ncol(table_df), border = fp_border_default
                (color="black",width=1), part = "header") %>%
      hline_top(j=1:ncol(table_df), border = fp_border_default
                (color="black",width=1)) %>%
      hline_bottom(j=1:ncol(table_df), border = 
                     fp_border_default(color="black",width=1)) %>%
      align(align = "center", part = "all", j = 2:ncol(table_df)) %>%
      bold(i=c(row_predictor,row_random_effects), j=1) %>%
      italic(i=row_predictor, j=c(3,4)) %>%
      italic(i=row_random_effects, j=4) %>%
      padding(i=c((row_predictor+1):(number_predictors+2)), j=1, padding.left=20) %>%
      padding(i=c((row_random_effects+1):(row_random_effects + number_random_effects)), j=1, padding.left=20) %>%
      line_spacing(space = 0.5, part = "body") %>%
      #merge_at(i=nrow(table_df)-1, j=1:ncol(table_df))%>%
      merge_at(i=nrow(table_df), j=1:ncol(table_df))%>%
      add_footer_lines(as_paragraph("Source data are provided as Source Data file.")) %>%
      padding(i=c(row_predictor, row_random_effects), padding.bottom=2) %>%
      fontsize(size=10, part="all") %>%
      autofit()
    ft
    
    if(save){
      table_path <- paste0(file_path, '/result_tables')
      
      if (!dir.exists(table_path)) {
        dir.create(table_path)
        cat("Directory created:", table_path, "\n")
      } else {
        cat("Directory already exists:", table_path, "\n")
      }
      
      full_file_path <- normalizePath(paste0(table_path,"/",file_name, ".docx"))
      save_as_docx(ft, path = full_file_path)
      
      # Conditionally open the created file
      if (show) {
        shell.exec(full_file_path)
      }
    }
    return(ft)
    
  }
  
  get_postHoc_LMM <- function(emmeans, eff) {
    # Initialize a data frame to hold the results
    results <- data.frame()
    emmeans <- as.data.frame(summary(emmeans)$contrast)
    eff = as.data.frame(eff)
    # Loop over unique contrast combinations
    for (row_i in 1:nrow(emmeans)) {
      contrast_df <- emmeans[row_i,]
      eff_size_df <- eff[row_i, ]
      # Construct the contrast string
      contrast_string <- as.character(contrast_df$contrast)
      
      # Add any additional grouping columns that might exist between 'contrast' and 'estimate'
      additional_columns <- setdiff(names(contrast_df), c("contrast", "estimate", "SE", "df", "t.ratio", "p.value"))
      if (length(additional_columns) > 0) {
        additional_values <- paste(contrast_df[, additional_columns], collapse = " in ")
        contrast_string <- paste(contrast_string, additional_values, sep = " in ")
      }
      
      
      # Extract the necessary columns
      result <- data.frame(
        contrast = contrast_string,
        t = contrast_df$t.ratio,
        df = contrast_df$df,
        d = eff_size_df$effect.size,
        lower_CI = eff_size_df$lower.CL,
        upper_CI = eff_size_df$upper.CL,
        p = contrast_df$p.value,
        stringsAsFactors = FALSE
      )
      
      # Add the results to the master data frame
      results <- rbind(results, result)
    }
    results <- round_two_spaces(table_df = results, start_col = 2, end_col = ncol(results) - 1) # Corrected `result` to `results`
    results <- round_p(table_df = results)
    return(results)
  }

# READ IN DATA####
  # path settings####
  # set working directory to where this script is stored
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
    getwd() # check
  # read in data from /data
    file_path <- file.path("../data") #run first the line above
      
  # main study ####
    # control analyses 
      controlDf <- read_excel(file.path(file_path, "behav", "controlMeasuresDf.xlsx"))%>%
                  mutate(delay = factor(delay), 
                         sex = factor(sex), 
                         Name = factor(Name)) 
    
    # behavioral data 
      behavDf <- read_excel(file.path(file_path, "behav","behavDf.xlsx")) %>%
                  mutate(emotion = factor(emotion, levels = c("neutral","negative")),
                         delay = factor(delay))
      
    # model RSA data 
      modelRSADf <- read_excel(file.path(file_path, "neuro", 'modelRSADf.xlsx')) %>% 
                     mutate(emotion = factor(emotion, levels=c('neutral','negative')),
                             Name = factor(Name), 
                             model = factor(model),
                             delay = factor(delay))
      
    # reinstatement data
      reinstatementDf <- read_excel(file.path(file_path, "neuro", "reinstatementDf.xlsx")) %>% 
        mutate(delay = factor(delay), 
               emotion = factor(emotion, levels=c('neutral','negative')), 
               Name = factor(Name)) 
      
  # pilot study ####
    
    # sociodemography
    pilotDemoBeforeExclusionDf <- read_excel(file.path(file_path, "pilot","pilotDemoDf.xlsx")) %>%
      mutate(sex = factor(sex))
    
    # all stimulus sets in pilot
    pilotAllSetsDf <- read_excel(file.path(file_path, "pilot","pilotAllSetsDf.xlsx")) %>% 
      mutate(emotion = factor(emotion),
             Name = factor(Name),
             lureType = factor(lureType, levels = c("new","sem","per")))
    
    # stimulus sets for main study
    pilotFinalSetsDf <- read_excel(file.path(file_path, "pilot","pilotFinalSetsDf.xlsx")) %>% 
      mutate(emotion = factor(emotion),
             Name = factor(Name),
             lureType = factor(lureType, levels = c("new","sem","per"), 
                               labels = c("unrelated","semantically related","perceptually related")),
             ratingScale = factor(ratingScale, levels = c("Per","Sem"), 
                                  labels = c("perceptual relatedness", "semantic relatedness"))) %>%
             aggregate(rating ~ Name + emotion + lureType + ratingScale, FUN = mean) # include sex for source data file
    
# FIGURE SETTINGS #####
  
  font_add_google("Encode Sans Condensed", "Encode Sans Condensed")
  showtext.auto()
  
  dodge = 0.15
  
  my_theme <- theme_classic()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          plot.title = element_text(hjust = 0.5, size=35, family = "Encode Sans Condensed"),
          axis.title.y=element_text(size=28, family="Encode Sans Condensed"),
          axis.text.y = element_text(size = 26, colour="black", family = "Encode Sans Condensed"),
          axis.title.x=element_text(size=28, family = "Encode Sans Condensed"),
          axis.text.x = element_text(size = 28, colour="black", family="Encode Sans Condensed"),
          legend.title = element_text(size = 28, family = "Encode Sans Condensed"),
          legend.text = element_text(size = 26, family = "Encode Sans Condensed"),
          axis.line =  element_line(linewidth=1.75),
          axis.ticks = element_line(linewidth=1.75, colour="black"),
          axis.ticks.length = unit(.2,"cm"),
          strip.text = element_text(size=28, family = "Encode Sans Condensed"),
          strip.background=element_rect(color="white"))   

# CREATE SOURCE DATA FILE ####
  wb <- createWorkbook("source_data.xlsx")
  
# GROUPS DID NOT DIFFER IN ATTENTIVENESS DURING ENCODING (DAY 1)####
  # prepare data ####
    longerEncRunDf <- controlDf %>%
                      dplyr::select(Name, delay, EncodingRun1_missedResponse, EncodingRun2_missedResponse, EncodingRun3_missedResponse)%>%
                      pivot_longer(cols = c(EncodingRun1_missedResponse, EncodingRun2_missedResponse, EncodingRun3_missedResponse),
                                            names_to = "run", # define new factor run
                                            values_to = "missedResponse")%>% # name values
                      mutate(run = factor(run, #define run factor and rename levels
                                          levels=c('EncodingRun1_missedResponse', 'EncodingRun2_missedResponse', 
                                                   'EncodingRun3_missedResponse'),
                                          labels=c('run1', 'run2', 'run3'))) 
  
  # analyze data ####
    # describe 
  
      meanDf <- aggregate(missedResponse ~ Name, FUN = sum,  data = longerEncRunDf)
      describe(meanDf$missedResponse)
      
    # ANOVA
      ANOVA <- aov_ez(
        "Name"
        ,"missedResponse"
        ,longerEncRunDf
        ,between=c("delay")
        ,within=c("run")
        ,anova_table="pes")
      summary(ANOVA)
      
      title <- "missed responses"
      table_df <- get_ANOVA_results(ANOVA = ANOVA)
      save_ANOVA_text(table_df=table_df, title, file_name = "output", show_file = TRUE) 

# EMOTIONAL ENHANCEMENT OF IMMEDIATE FREE RECALL (DAY 1)####
  # prepare data ####
    freeRecallDf <- aggregate(freeRecall ~ Name + delay + emotion, FUN = sum, 
                                      na.rm = TRUE, na.action = na.pass, 
                                      subset(behavDf, itemType == 'old'))%>% 
                                      mutate(freeRecall = freeRecall / 30 * 100) # get percent
      
    meanDf <- aggregate(freeRecall ~ Name + delay, FUN = sum, 
                        na.rm = TRUE, na.action = na.pass, 
                        subset(behavDf, itemType == 'old'))%>% 
                        mutate(freeRecall = freeRecall / 60 * 100) # get percent

  # analyze data ####
    # descriptive statistics
      #overall free recall geradless of delay and emotion
      describe(meanDf$freeRecall)
      #free recall per group and emotion
      psych::describeBy(freeRecall ~ emotion + delay, data = freeRecallDf)
      #free recall per emotion regardless of group
      psych::describeBy(freeRecall ~ emotion, data = freeRecallDf)
    
    # run ANOVA
      ANOVA <- aov_ez(
        "Name"
        ,"freeRecall"
        ,freeRecallDf 
        ,between=c("delay")
        ,within=c("emotion")
        ,anova_table="pes")
      print(ANOVA)
      
      title <- "free recall"
      table_df <- get_ANOVA_results(ANOVA = ANOVA)
      save_ANOVA_text(table_df=table_df, title, file_name = "output", show_file = TRUE) 
      
    # post-hoc tests
      emmeans <- emmeans(ANOVA, pairwise ~ emotion, adjust="sidak")#difference between emotion averaged over delay
      table_df <- compute_effect_size_paired(emeans=emmeans, data = freeRecallDf, 
                                             group_variable = NULL, within_variable = "emotion",
                                             response_variable = "freeRecall")
      save_postHoc_t_text(table_df = table_df, start_col =2, header_col = title)
      
  # Supplementary Figure 2####
    
      svg("S1_freeRecall.svg")
      
      plot_data <- freeRecallDf %>% #for connected data points
        mutate(x = case_when(emotion == "neutral" ~ 1 - dodge,
                             emotion == "negative" ~ 2 + dodge))
      
      p <- ggplot(data = plot_data, aes(x = emotion, y = freeRecall, fill = emotion)) +
                  stat_summary(fun = 'mean',geom = 'bar',position = position_dodge(56),
                  size = 1,width = 1.42) +
        geom_point(pch = 19,position = position_dodge(6),alpha = 0.2,size = 3) +
        geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1)
      p + stat_summary(fun.data = mean_se,geom = "errorbar",position = position_dodge(0.6),
          width = 0,size = 1.7) +
        scale_x_discrete(labels = c("1d", "28d")) +
        labs(y = "immediate free recall (%)") +
        scale_fill_manual(values = c("azure4", "firebrick4")) +
        #annotation
        #line
        annotate("path",x = c(1.015, 1.985),y = c(85, 85),size = 1.5) +
        # stars
        annotate("text",x = 1.5,y = 87,label = "* * *",size = 15) + 
        coord_cartesian(ylim = c(0, 100)) +
        my_theme
      
      dev.off()
  
    # add to source_data ####
      # add sex to data frame to include in source data frame    
      subset_df <- merge(freeRecallDf, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file 
      # add new worksheet
      addWorksheet(wb, "SupplementaryFigure2")
      # Write the data into sheet
      writeData(wb, "SupplementaryFigure2", subset_df)
      
# DELAY DEPENDENT INCREASE IN HITS OVER TIME (DAY 2) #####
  # prepare data####
    # reduce dataframe 
      oldsDf <- aggregate(cbind(hit, miss, missedResponse) ~ Name + delay + emotion, 
                          FUN = sum, na.rm = TRUE, na.action = na.pass, 
                          data = subset(behavDf, itemType == 'old'))
    # compute percent 
      oldsDf[,c("hit","miss","missedResponse")] <- oldsDf[,c("hit","miss"
                                                             ,"missedResponse")] /30*100
  # analyze data ####
    # descriptive statistics 
      psych::describeBy(hit ~ delay, 
                        data = oldsDf)
      
      psych::describeBy(oldsDf$hit, oldsDf$delay) #hits depending on delay-group
      
      psych::describeBy(cbind(hit, miss, missedResponse) ~ emotion + delay, 
                        data = oldsDf)
      
      # new meanDf for cohens d
      meanDf <- aggregate(hit ~ Name + delay, FUN = mean, data = oldsDf) 
  
    # run ANOVA
      ANOVA <- aov_ez(
        "Name"
        ,"hit"
        ,oldsDf
        ,between=c("delay")
        ,within=c("emotion")
        ,anova_table="pes")
    
     title = "hit"  
     table_df <- get_ANOVA_results(ANOVA=ANOVA)
     save_ANOVA_text(table_df, title = title)

    # post-hoc tests
      ##difference between emotions separately for each group
      emmeans <- emmeans(ANOVA, pairwise ~ emotion|delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      
      new_df <- compute_effect_size_paired(emmeans, data=oldsDf, group_variable='delay', 
                          within_variable='emotion',response_variable='hit') 
      table_df <- new_df
      
      # difference between delay averaging over emotion
      emmeans <- emmeans (ANOVA, pairwise ~ delay, lmer.df = "satterthwaite",adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      stat <- as.data.frame(summary(emmeans)$contrasts) #sidak-adjustment if necessary
      new_df <- compute_effect_size(emmeans, stat)
      table_df <- rbind(table_df, new_df)
      
      #interaction contrast: difference in delay-dependent change in hits between emotions
      emmeans <- emmeans(ANOVA, specs = ~ delay*emotion, lmer.df = "satterthwaite")
      emmeans
      
      neut.1d = c(1, 0, 0, 0)
      neut.28d = c(0, 1, 0, 0)
      neg.1d = c(0, 0, 1, 0)
      neg.28d = c(0, 0, 0, 1)
      
      # interaction contrast
      stat <- pairs(contrast(emmeans, method = list("neut 1d - neut 28d" = neut.1d - neut.28d,
                                   "neg 1d - neg 28d" = neg.1d - neg.28d)),adjust="sidak")
      new_df <- compute_effect_size_interaction(emmeans, stat)
      # Appending the new results to the existing data frame
      table_df <- rbind(table_df, new_df)

      save_postHoc_t_text(table_df=table_df, start_col = 2, header_row = title, round = TRUE)
     
  # check influence of outliers ####
    # check outlier 
      data <- oldsDf
      
      #1d - neutral
      log1 <- which(data$hit[data$delay == "1d" & data$emotion == "neutral"]  > (mean(data$hit[data$delay == "1d" & data$emotion == "neutral"])+3*sd(data$hit[data$delay == "1d" & data$emotion == "neutral"]))|  data$hit[data$delay == "1d" & data$emotion == "neutral"]  < (mean(data$hit[data$delay == "1d" & data$emotion == "neutral"])+ (-3*sd(data$hit[data$delay == "1d" & data$emotion == "neutral"]))))
      data$hit[data$delay == "1d" & data$emotion == "neutral"][log1] = 999
      #no outlier
      
      #28d - neutral
      log1 <- which(data$hit[data$delay == "28d" & data$emotion == "neutral"]  > (mean(data$hit[data$delay == "28d" & data$emotion == "neutral"])+3*sd(data$hit[data$delay == "28d" & data$emotion == "neutral"]))|  data$hit[data$delay == "28d" & data$emotion == "neutral"]  < (mean(data$hit[data$delay == "28d" & data$emotion == "neutral"])+ (-3*sd(data$hit[data$delay == "28d" & data$emotion == "neutral"]))))
      data$hit[data$delay == "28d" & data$emotion == "neutral"][log1] = 999
      #no outlier
      
      #1d - negative
      log1 <- which(data$hit[data$delay == "1d" & data$emotion == "negative"]  > (mean(data$hit[data$delay == "1d" & data$emotion == "negative"])+3*sd(data$hit[data$delay == "1d" & data$emotion == "negative"]))|  data$hit[data$delay == "1d" & data$emotion == "negative"]  < (mean(data$hit[data$delay == "1d" & data$emotion == "negative"])+ (-3*sd(data$hit[data$delay == "1d" & data$emotion == "negative"]))))
      data$hit[data$delay == "1d" & data$emotion == "negative"][log1] = 999
      #no outlier
      
      #28d - negative
      log1 <- which(data$hit[data$delay == "28d" & data$emotion == "negative"]  > (mean(data$hit[data$delay == "28d" & data$emotion == "negative"])+3*sd(data$hit[data$delay == "28d" & data$emotion == "negative"]))|  data$hit[data$delay == "28d" & data$emotion == "negative"]  < (mean(data$hit[data$delay == "28d" & data$emotion == "negative"])+ (-3*sd(data$hit[data$delay == "28d" & data$emotion == "negative"]))))
      data$hit[data$delay == "28d" & data$emotion == "negative"][log1] = 999
      #no outlier
      
      #no outlier!
    
  # Figure 2A left#####
    # define and save figure
      svg("2_hits.svg")
      
      plot_data <- oldsDf %>% ##for connected individual data points
        mutate(x = case_when(
          delay == "1d" & emotion == "neutral" ~ 1 - dodge,
          delay == "1d" & emotion == "negative" ~ 1 + dodge,
          delay == "28d" & emotion == "neutral" ~ 2 - dodge,
          delay == "28d" & emotion == "negative" ~ 2 + dodge,
        ))
      
      
      p <- ggplot(data=plot_data, aes(x=delay, y=hit, fill=emotion))+
                  stat_summary(fun='mean',geom='bar', position=position_dodge())+
                  geom_point(pch = 19, position = position_dodge(0.6),  
                             alpha = 0.2, size=3)+
        geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
      p + 
        stat_summary(fun.data = mean_se, geom = "errorbar",  
                     position=position_dodge(0.9),width = 0, size = 1.7)+
        scale_x_discrete(labels=c("1d", "28d"))+
        labs(y="% hits in delayed recognition")+
        scale_fill_manual(values=c("azure4", "firebrick4"))+
        # stars 
        annotate("text", x = 2, y = 87, label = "* *", color="black", size =15)+ 
        #line
        annotate("path", x = c(1.79, 2.21), y = c(85, 85), size=1.5) +
        # stars 
        annotate("text", x = 1.5, y = 101.00, label = "* * *", color="black", size = 15)+ 
        #line
        annotate("path", x = c(1, 2), y = c(99, 99), size=1.5) +
        coord_cartesian( ylim = c(0, 100)) +
        my_theme
      dev.off()
  
  # add to source_data Figure 2a left ####
      addWorksheet(wb, "Figure2A_left")
      # Write the data into sheet
      subset_df <- subset(oldsDf, select = c("Name", "delay", "emotion", "hit"))
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file 
      
      writeData(wb, "Figure2A_left", subset_df)
      
  # Supplementary Table X ####
    # old items 
      # reduce data frame
      oldRawDf <- aggregate(cbind(hit, miss, missedResponse) ~ Name + delay + emotion, FUN = sum, 
                                  na.rm = TRUE, na.action = na.pass, 
                                  data = subset(behavDf, itemType == 'old')) 
      # show descriptive statistics for old items
      psych::describeBy(cbind(hit, miss, missedResponse) ~ emotion + delay, 
                        data = oldRawDf) 
    
    # lures 
      # reduce data frame
      luresRawDf <- aggregate(cbind(FA, CR, missedResponse) ~ Name + delay + emotion + itemType, 
                                      FUN = sum, na.rm = TRUE, na.action = na.pass, 
                                      data = subset(behavDf, itemType != 'old')) 
      # change level order of itemType
      luresRawDf$itemType <- factor(luresRawDf$itemType, levels = c("per", "sem", "new"))
      # show descriptive statistics for lures
      psych::describeBy(cbind(CR, FA, missedResponse) ~ emotion + delay +  itemType, data = luresRawDf)  
      
    # add to source_data ####
      # old items
      addWorksheet(wb, "SupplementaryTable1_oldItems")
      # Write the data into sheet
      subset_df <- merge(oldRawDf, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file 
      writeData(wb, "SupplementaryTable1_oldItems", subset_df)
      
      # lures
      subset_df <- merge(luresRawDf, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      # Define a named character vector of old and new level names
      level_names <- c("per" = "perceptually related", "sem" = "semantically related", "new" = "unrelated")
      # Use the plyr package's mapvalues function to rename levels
      subset_df$lureType <- plyr::mapvalues(subset_df$itemType, from = names(level_names), to = level_names)
      # add worksheet
      addWorksheet(wb, "SupplementaryTable1_lures")
      # Write the data into sheet
      writeData(wb, "SupplementaryTable1_lures", subset_df)
      
# DELAY DEPENDENT INCREASE IN FALSE ALARMS SPECIFICALLY FOR SEMANTICALLY RELATED LURES (DAY 2) ####
  # prepare data ####
    # reduce dataframe 
    luresDf <- aggregate(FA ~ Name + delay + emotion + itemType, FUN = sum, 
                         na.rm = TRUE, na.action = na.pass, 
                         data = subset(behavDf, itemType != 'old'))%>% # take only lures 
              mutate(FA = FA /30*100,# get percent
                     lureType = factor(itemType, levels = c("new","per","sem"))) #new=unrelated, #per=perceptually related, #sem=semantically related
      # compute cohens d over factor emotion
      meanDf <- aggregate(FA ~ Name + delay + lureType, FUN = function(x) mean(x, na.rm = TRUE), 
                          data = luresDf)
      
  # analyze data ####
    # run ANOVA #####
      ANOVA <- aov_ez(
        "Name"
        ,"FA"
        ,luresDf
        ,between=c("delay")
        ,within=c("emotion","lureType")
        ,anova_table="pes")
      
      table_df <- get_ANOVA_results(ANOVA)
      save_ANOVA_text(table_df, title="FAs In Recognition")
  
    # post-hoc tests ##########
      # 
      title <- "delay effect per lureType"
      emmeans <- emmeans (ANOVA, pairwise ~ delay|lureType, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      stat  <- summary(emmeans)$contrasts 
      
      new_df <- compute_effect_size(emmeans, stat)
      # Adding the title column
      new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
      table_df <- new_df
      
      #
      title <- "lureType effect per delay"
      emmeans <- emmeans (ANOVA, pairwise ~ lureType|delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      stat  <- summary(emmeans)$contrasts 
      new_df <- compute_effect_size_paired(emmeans = emmeans, group_variable = "delay", 
                                 within_variable = "lureType", data = luresDf, response_variable = "FA")
      # Adding the title column
      new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
      # add to results table
      table_df <- rbind(table_df, new_df)
      table_df
      
      #interaction contrast: difference increase in FAs over time between lure types
      title = "difference in delay effect between lureTypes"
      emmeans <- emmeans(ANOVA, specs = ~ delay*lureType, lmer.df = "satterthwaite")
      summary(emmeans) # to see order of conditions and use them for contrast
      
      sem.1d = c(0, 0, 0, 0, 1, 0)
      sem.28d = c(0, 0, 0, 0, 0, 1)
      new.1d = c(1, 0, 0, 0, 0, 0)
      new.28d = c(0, 1, 0, 0, 0, 0)
      per.1d = c(0, 0, 1, 0, 0, 0)
      per.28d = c(0, 0, 0, 1, 0, 0)
      
      stat <- pairs(contrast(emmeans, method = list("sem 1d - sem 28d" = sem.1d - sem.28d,
                                                   "new 1d - new 28d" = new.1d - new.28d,
                                                   "per 1d - per 28d" = per.1d - per.28d)),adjust="sidak") 
      # compute effect size with ci and write all into table
      new_df <- compute_effect_size_interaction(emmeans, stat, model = ANOVA)
      # Adding the title column
      new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
      # add to results table
      table_df <- rbind(table_df, new_df)
      table_df
      
      # paired t-test      
      title = "emotion effect per lureType and delay"
      emmeans <- emmeans(ANOVA, pairwise ~ emotion|lureType:delay, lmer.df = "satterthwaite", adjust="sidak")#satterwhaite for fastening up computation, does not change results of contrasts
      stat  <- summary(emmeans)$contrasts 
      new_df <- compute_effect_size_paired(emmeans=emmeans, data=luresDf, group_variable=c("delay","lureType"),
                                        within_variable="emotion", response_variable="FA")
      # Adding the title column
      new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
      # add to results table
      table_df <- rbind(table_df, new_df)
      table_df
      
      # paired t-test
      title <- "lureType effect per emotion and delay"
      emmeans <- emmeans(ANOVA, pairwise ~ lureType|emotion:delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      new_df <- compute_effect_size_paired(emmeans=emmeans, data=luresDf, group_variable=c("delay","emotion"),
                                           within_variable="lureType", response_variable="FA")
      
      # Adding the title column
      new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
      # add to results table
      table_df <- rbind(table_df, new_df)
      table_df
    
      #interaction contrast
      title <- "emotional enhancement of semantization over time"
      emmean <- emmeans(ANOVA, specs = ~ emotion*lureType*delay, lmer.df = "satterthwaite")
      summary(emmean) # to see order of conditions and use them for contrast
      # order of conditions:
      #1 neutral  new      1d      
      #2 negative new      1d      
      #3 neutral  per      1d      
      #4 negative per      1d     
      #5 neutral  sem      1d - > we need this         
      #6 negative sem      1d - > we need this    
      #7 neutral  new      28d   
      #8 negative new      28d     
      #9 neutral  per      28d     
      #10 negative per      28d     
      #11 neutral  sem      28d - > we need this       
      #12 negative sem      28d - > we need this       
      
      sem_neut_1d = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
      sem_neg_1d = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
      sem_neut_28d = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
      sem_neg_28d = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    

      title <- "emotion effect in sem per delay"
      stat <- pairs(contrast(emmean, method = list("sem neut 1d - sem neg 1d" = sem_neut_1d - sem_neg_1d,
                                                   "sem neut 28d - sem neg 28d" = sem_neut_28d - sem_neg_28d)), adjust="sidak") 
      new_df <- compute_effect_size_interaction(emmeans, stat, model = ANOVA)
      # Adding the title column
      new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
      # add to results table
      table_df <- rbind(table_df, new_df)
      table_df
      
      # two-sample t-test
      title <- "delay effect per lureType and emotion"
      emmeans <- emmeans (ANOVA, pairwise ~ delay|lureType:emotion, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      stat  <- summary(emmeans)$contrasts #sidak-adjustment if necessary
      new_df <- compute_effect_size(emmeans=emmeans,  model = ANOVA)
      # Adding the title column
      new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
      # add to results table
      table_df <- rbind(table_df, new_df)
      table_df

      save_postHoc_t_text(table_df = table_df, header_row = "posthoc FAs", start_col = 3)

      # post-hoc ANOVAs to check whether the emotion x delay interaction depends on lure type
        # ANOVA for semantically related only
         ANOVA <- aov_ez(
            "Name"
            ,"FA"
            ,subset(luresDf, lureType == "sem")
            ,between=c("delay")
            ,within=c("emotion")
            ,anova_table="pes")
          
         table_df <- get_ANOVA_results(ANOVA)
         save_ANOVA_text(table_df, title = "emo x delay anova in sem")
        
        # ANOVA for unrelated only
          ANOVA <- aov_ez(
            "Name"
            ,"FA"
            ,subset(luresDf, lureType == "new")
            ,between=c("delay")
            ,within=c("emotion")
            ,anova_table="pes")
          
          table_df <- get_ANOVA_results(ANOVA)
          save_ANOVA_text(table_df, title = "emo x delay anova in new")
        
        # ANOVA for perceptually related only
          ANOVA <- aov_ez(
            "Name"
            ,"FA"
            ,subset(luresDf, lureType == "per")
            ,between=c("delay")
            ,within=c("emotion")
            ,anova_table="pes")
          
          table_df <- get_ANOVA_results(ANOVA)
          save_ANOVA_text(table_df, title = "emo x delay anova in per")
          
  # check influence of outliers ####
    # find outliers depending on each condition 
      data <- luresDf # copy dataframe
      
      #per
      log1 <- which(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))))
      data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
      # 1 outlier
      
      log1 <- which(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]))))
      data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
      #none
      
      log1 <- which(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))|  data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))))
      data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
      #none
      
      log1 <- which(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]))|  data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]))))
      data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
      #none
      
      #sem
      log1 <- which(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))))
      data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
      #none
      
      log1 <- which(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]))))
      data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
      #none
      
      log1 <- which(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))))
      data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
      # 1 
      
      log1 <- which(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]))))
      data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
      #none
      
      
      #new
      log1 <- which(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))))
      data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
      # 1
      
      log1 <- which(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]))))
      data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
      # 1
      
      log1 <- which(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))|  data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))))
      data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
      # 1
      
      log1 <- which(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]))|  data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]))))
      data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
      # 1
      
    # exclude outliers 
      log = data$FA == 999
      data$FA[log] = NA
      
    # run ANOVA after the exclusion of those outliers 
      FAs.ANOVA <- aov_ez(
        "Name"
        ,"FA"
        ,data
        ,between=c("delay")
        ,within=c("emotion","lureType")
        ,anova_table="pes")
      print(FAs.ANOVA) # no change due to outlier exclusion: interaction of delay x lureType and delay x emotion x lure Type still significant!
      summary(FAs.ANOVA)
    
  # Figure 2A right####
      
    svg("Figure2A_unrelatedLures.svg")
    
    plot_data <- luresDf %>% 
      filter(lureType == "new")%>% 
      mutate(x = case_when(
        delay == "1d" & emotion == "neutral" ~ 1 - dodge,
        delay == "1d" & emotion == "negative" ~ 1 + dodge,
        delay == "28d" & emotion == "neutral" ~ 2 - dodge,
        delay == "28d" & emotion == "negative" ~ 2 + dodge,
      ))
    
    p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
      stat_summary(fun='mean',geom='bar', position=position_dodge())+
      geom_point(pch = 19, position = position_dodge(0.6), 
                 alpha = 0.2, size=3)+
      geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
    p + 
      stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
      scale_x_discrete(labels=c("1d", "28d"))+
      scale_fill_manual(values=c("azure4", "firebrick4"))+
      geom_segment(aes(x=1,xend=2,y=20,yend=20), size=1.5) + 
      annotate(geom="text", x=1.5, y=21.25, label= c("*"), color="black", fontface = "bold", size = 15) +
      labs(y="false alarms in delayed recognition (%)")+coord_cartesian( ylim = c(0, 40)) +
      my_theme
    
    dev.off()
  
    svg("Figure2A_perceptuallyRelatedLures.svg")
    
    plot_data <- luresDf %>% 
      filter(lureType == "per") %>% 
      mutate(x = case_when(
        delay == "1d" & emotion == "neutral" ~ 1 - dodge,
        delay == "1d" & emotion == "negative" ~ 1 + dodge,
        delay == "28d" & emotion == "neutral" ~ 2 - dodge,
        delay == "28d" & emotion == "negative" ~ 2 + dodge,
      ))
    p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
      stat_summary(fun='mean',geom='bar', position=position_dodge())+
      geom_point(pch = 19, position = position_dodge(0.6), 
                 alpha = 0.2, size=3)+
      geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
    p + 
      stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
      scale_x_discrete(labels=c("1d", "28d"))+
      scale_fill_manual(values=c("azure4", "firebrick4"))+
      labs(y="false alarms in delayed recognition (%)")+coord_cartesian( ylim = c(0, 40)) +
      my_theme
    
    dev.off()
  
  svg("Figure2A_semanticallyRelatedLures.svg")
  
  plot_data <- luresDf %>% 
    filter(lureType == "sem") %>% 
    mutate(x = case_when(
      delay == "1d" & emotion == "neutral" ~ 1 - dodge,
      delay == "1d" & emotion == "negative" ~ 1 + dodge,
      delay == "28d" & emotion == "neutral" ~ 2 - dodge,
      delay == "28d" & emotion == "negative" ~ 2 + dodge,
    ))
  
  p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
    stat_summary(fun='mean',geom='bar', position=position_dodge())+
    geom_point(pch = 19, position = position_dodge(0.6), 
               alpha = 0.2, size=3)+
    geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
  p + 
    stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
    scale_x_discrete(labels=c("1d", "28d"))+
    scale_fill_manual(values=c("azure4", "firebrick4"))+
    
    annotate(geom="text", x=1.5, y=25.25, label= c("* *"), color="black", fontface = "bold", size = 15) +
    geom_segment(aes(x=1,xend=2,y=24,yend=24), size=1.5) +
    
    annotate(geom="text", x=2, y=21.25, label= c("* *"), color="black", fontface = "bold", size = 15) +
    geom_segment(aes(x=1.75,xend=2.25,y=20,yend=20), size=1.5) +  
    
    labs(y="false alarms in delayed recognition (%)")+coord_cartesian( ylim = c(0, 40)) +
    my_theme
  
  dev.off()
  

  # add to source_data ####
    # Write the data into sheet
    subset_df <- subset(luresDf, select = -itemType)
    # Define a named character vector of old and new level names
    level_names <- c("per" = "perceptually related", "sem" = "semantically related", "new" = "unrelated")
    # Use the plyr package's mapvalues function to rename levels
    subset_df$lureType <- plyr::mapvalues(subset_df$lureType, from = names(level_names), to = level_names)
    # lures
    subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
    # add to source_data
    addWorksheet(wb, "Figure2A_right")
    writeData(wb, "Figure2A_right", subset_df)
  # false alarms weighted by confidence ####
      # preprare data 
        luresWeightedDf <- subset(behavDf, itemType != 'old') %>% # take only lures
                           aggregate(FA_weighted ~ Name + delay + emotion + itemType, 
                                     FUN = sum, na.rm = TRUE, na.action = na.pass) %>%
                           mutate(FA = FA_weighted/60*100, # compute percent
                                  lureType = factor(itemType, levels = c("new","per","sem")) )
  
      # run ANOVA 
        ANOVA <- aov_ez(
          "Name"
          ,"FA"
          ,luresWeightedDf
          ,between=c("delay")
          ,within=c("emotion","lureType")
          ,anova_table="pes")
        
        table_df <- get_ANOVA_results(ANOVA)
        save_ANOVA_text(table_df, title = "FAs weighted by confidence")

      # post-hoc tests
        #effect of delay depending on lure type
        emmeans <- emmeans (FAs.ANOVA, pairwise ~ delay|lureType, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans, adjust="sidak") #sidak-adjustment if necessary
        
        #interaction contrast: difference increase in FAs over time between lure types
        emmeans <- emmeans(ANOVA, specs = ~ delay*lureType, lmer.df = "satterthwaite")
        summary(emmeans)
        
        sem.1d = c(0, 0, 0, 0, 1, 0)
        sem.28d = c(0, 0, 0, 0, 0, 1)
        new.1d = c(1, 0, 0, 0, 0, 0)
        new.28d = c(0, 1, 0, 0, 0, 0)
        per.1d = c(0, 0, 1, 0, 0, 0)
        per.28d = c(0, 0, 0, 1, 0, 0)
        
        stats <- pairs(contrast(emmeans, method = list("sem 1d - sem 28d" = sem.1d - sem.28d,
                                                  "new 1d - new 28d" = new.1d - new.28d,
                                                  "per 1d - per 28d" = per.1d - per.28d)),adjust="sidak") 
        
        table_df <- compute_effect_size_interaction(stat=stats,emmeans = emmeans, model = ANOVA, title = "interaction")
        
        save_postHoc_t_text(table_df=table_df,header_row = "interaction contrast weighted FAs")
        
        emmeans <- emmeans (ANOVA, pairwise ~ emotion|lureType:delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans)
        
        
        #for plotting
        #differences between lure types depending on delay
        emmeans <- emmeans(ANOVA, pairwise ~ lureType|delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans) #sidak-adjustment if necessary
  
  # Supplementary Figure 6 ######
    
    svg("SupplementaryFigWeighted_unrelatedLures.svg")
    
    plot_data <- luresWeightedDf %>% 
                  filter(lureType == "new")%>% 
                  mutate(x = case_when(
                    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
                    delay == "1d" & emotion == "negative" ~ 1 + dodge,
                    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
                    delay == "28d" & emotion == "negative" ~ 2 + dodge,
                  ))
        
    p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
                  stat_summary(fun='mean',geom='bar', position=position_dodge())+
                  geom_point(pch = 19, position = position_dodge(0.6), 
                             alpha = 0.2, size=3)+
                  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
    p + 
      stat_summary(fun.data = mean_se, geom = "errorbar",  
                   position=position_dodge(0.9),width = 0, size = 1.7)+
      scale_x_discrete(labels=c("1d", "28d"))+
      scale_fill_manual(values=c("azure4", "firebrick4"))+
      labs(y="% false alarms (weighted by confidence)")+
      coord_cartesian( ylim = c(0, 40)) +
      my_theme
    dev.off()
    
    svg("SupplementaryFigWeighted_perceptuallyRelatedLures.svg")
    
    plot_data <- luresWeightedDf %>% 
      filter(lureType == "per") %>% 
      mutate(x = case_when(
        delay == "1d" & emotion == "neutral" ~ 1 - dodge,
        delay == "1d" & emotion == "negative" ~ 1 + dodge,
        delay == "28d" & emotion == "neutral" ~ 2 - dodge,
        delay == "28d" & emotion == "negative" ~ 2 + dodge,
      ))
    
    p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
      stat_summary(fun='mean',geom='bar', 
                   position=position_dodge())+
      geom_point(pch = 19, position = position_dodge(0.6), 
                 alpha = 0.2, size=3)+
      geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
    p + 
      stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),
                   width = 0, size = 1.7)+
      scale_x_discrete(labels=c("1d", "28d"))+
      scale_fill_manual(values=c("azure4", "firebrick4"))+
      labs(y="% false alarms (weighted by confidence)")+coord_cartesian( ylim = c(0, 40)) +
      theme_classic()+
      my_theme
    
    dev.off()
    
    svg("SupplementaryFigWeighted_semanticallyRelatedLures.svg")
    
    plot_data <- luresWeightedDf %>% 
      filter(lureType == "sem") %>% 
      mutate(x = case_when(
        delay == "1d" & emotion == "neutral" ~ 1 - dodge,
        delay == "1d" & emotion == "negative" ~ 1 + dodge,
        delay == "28d" & emotion == "neutral" ~ 2 - dodge,
        delay == "28d" & emotion == "negative" ~ 2 + dodge,
      ))
    
    p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
      stat_summary(fun='mean',geom='bar', 
                   position=position_dodge())+
      geom_point(pch = 19, position = position_dodge(0.6), 
                 alpha = 0.2, size=3)+
      geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
    p + 
      stat_summary(fun.data = mean_se, geom = "errorbar",  
                   position=position_dodge(0.9),width = 0, size = 1.7)+
      scale_x_discrete(labels=c("1d", "28d"))+
      scale_fill_manual(values=c("azure4", "firebrick4"))+
      # delay effect
      annotate(geom="text", x=1.5, y=25.25, label= c("* *"), color="black", size = 15) +
      geom_segment(aes(x=1,xend=2,y=24,yend=24), size=1.5) +
      #emotion effetc at 28d
      annotate(geom="text", x=2, y=21.25, label= c("* * *"), color="black",  size = 15) +
      geom_segment(aes(x=1.75,xend=2.25,y=20,yend=20), size=1.5) +  
      
      labs(y="% false alarms (weighted by confidence)") + 
      coord_cartesian( ylim = c(0, 40)) +
      my_theme
    
    dev.off()
  
  
    # write Supplementary Figure 6 into source_data ####
      # Write the data into sheet
      subset_df <- luresWeightedDf[, c("Name", "delay", "emotion", "lureType","FA_weighted")]
      # Define a named character vector of old and new level names
      level_names <- c("per" = "perceptually related", "sem" = "semantically related", "new" = "unrelated")
      # Use the plyr package's mapvalues function to rename levels
      subset_df$lureType <- plyr::mapvalues(subset_df$lureType, from = names(level_names), to = level_names)
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      # add to source_data
      addWorksheet(wb, "SupplementaryFigure6")
      writeData(wb, "SupplementaryFigure6", subset_df)
    # confidency of FAs ####
      # semantically related lures 
        # prepare data
          semConfDf <- subset(behavDf, itemType == 'sem') %>%
            dplyr::select(Name, delay, stimulusTypeNum, emotion, FA_confidency)
        # analyze data
          gLMM <- glmer(FA_confidency ~ delay * emotion +
                          (1| Name)+(1|stimulusTypeNum), data = semConfDf, family = "binomial", 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5))) 
          summary(gLMM)
          
          # Check for NA or NaN values
          missing_values <- is.na(semConfDf$FA_confidency) | is.nan(semConfDf$FA_confidency)
          
          # Print a summary of the missing values
          if (any(missing_values)) {
            print("Missing or NaN values found in FA_confidency:")
            print(semConfDf[missing_values, "FA_confidency"]) # Print the specific missing or NaN values
          } else {
            print("No missing or NaN values found in FA_confidency.")
          }
          
          n_distinct(semConfDf$Name)
          
          file_name <- "gLMM_FA_confidency_sem"
          result <- get_gLMM_results(gLMM=gLMM, title=title)
          create_gLMM_table(result, file_name = "semConf")
          
      # perceptually related lures 
        # prepare data 
          perConfDf <- subset(behavDf, itemType == 'per') %>%
            dplyr::select(Name, delay, stimulusTypeNum, emotion, FA_confidency)
        # analyze data
          gLMM <- glmer(FA_confidency ~ delay * emotion +
                          (1| Name)+(1|stimulusTypeNum), data = perConfDf, family = "binomial", 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5))) 
          
          file_name <- "gLMM_FA_confidency_per"
          result <- get_gLMM_results(gLMM=gLMM, title=file_name)
          create_gLMM_table(result, file_name = file_name)
          
          
      # unrelated lures 
        # prepare data 
          newConfDf <- subset(behavDf, itemType == 'new') %>%
            dplyr::select(Name, delay, stimulusTypeNum, emotion, FA_confidency)
          
          n_distinct(newConfDf$Name)
        # run LMM
          gLMM <- glmer(FA_confidency ~ delay * emotion +
                          (1| Name), data = newConfDf, family = "binomial", 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5))) 
          
          file_name <- "gLMM_FA_confidency_new"
          result <- get_gLMM_results(gLMM=gLMM, title=file_name)
          create_gLMM_table(result, file_name = file_name)
      # add Supplementary Table 2 ####
          subset_df <- subset(behavDf, itemType != 'old') %>%
            dplyr::select(Name, delay, stimulusTypeNum, emotion, FA_confidency, itemType)
          subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
          
          # for supplementary table
          addWorksheet(wb, "SupplementaryTable2")
          writeData(wb, "SupplementaryTable2", subset_df)

# MEMORY SPECIFICITY ON THE LEVEL OF EACH INDIVIDUAL RELATED STIMULUS SET #####
  # prepare data ####
    # reduce data frame without aggregating 
      transformationDf <- subset(behavDf, itemType == "old") %>%
                          dplyr::select ("Name", "set", "delay", "emotion", "detailed", "forgotten", 
                                        "semOnly_transformed", "perOnly_transformed") %>%
                          mutate(detailed = factor(detailed),
                                 forgotten = factor(forgotten),
                                 semOnly_transformed = factor(semOnly_transformed),
                                 perOnly_transformed = factor(perOnly_transformed),
                                 set = factor(set) )
          
    # prepare smaller df for plotting and descriptive statistics in perc
      smaller_transformationDf <- subset(behavDf, itemType == "old") %>%
                                  dplyr::select ("Name", "set", "delay", "emotion", "detailed", "forgotten", 
                                                 "semOnly_transformed", "perOnly_transformed") %>%
                                  mutate(detailed = as.numeric(detailed),
                                         forgotten = as.numeric(forgotten),
                                         semOnly_transformed = as.numeric(semOnly_transformed),
                                         perOnly_transformed = as.numeric(perOnly_transformed)) %>% 
                                  aggregate(cbind(detailed, forgotten, semOnly_transformed, 
                                                  perOnly_transformed) ~ Name + delay + emotion, 
                                                  FUN = sum, na.rm = TRUE, na.action = na.pass)

  # analyze data ####
    # descriptive statistics
      # average of missing data in data frame
      transformationDf["missingTransCategory"] <- rowMeans(is.na(transformationDf[, c("detailed", "semOnly_transformed", "forgotten", "perOnly_transformed")]))
      
      meanDf <- aggregate(missingTransCategory ~ Name + delay, FUN = sum, data = transformationDf)
      meanDf["missingTransCategory_perc"] <- meanDf["missingTransCategory"]/60*100
      
      describe(meanDf$missingTransCategory_perc)
      
      head(meanDf)
      
      # no difference between groups in missing categories
      t.test(missingTransCategory ~ delay, data = meanDf)
      cohen.d(meanDf$missingTransCategory, meanDf$delay) # from library effsize or psych # for d and ci
      
      
      # SHOW MEAN AND SEM IN PERCENT
      psych::describeBy(cbind(detailed, forgotten, semOnly_transformed, 
                              perOnly_transformed) ~ emotion + delay, 
                              data = smaller_transformationDf)
      
    # detailed memories #####
      #fit binomial generalized linear mixed model on detailed memory 
      gLMM <- glmer(detailed ~ delay * emotion +
                               (1| Name)+(1|set), data = transformationDf, family = "binomial", 
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl=list(maxfun=2e5)))
      summary(gLMM)
      file_name <- "gLMM_detailed"
      result <- get_gLMM_results(gLMM=gLMM, title=file_name)
      create_gLMM_table(result, file_name = file_name)
      
      # post-hoc test #####
      # two sample
      title <- "delay effect in detailed"
      emmeans <- emmeans(gLMM, pairwise ~ delay, lmer.df = "satterthwaite", adjust="sidak") #1d vs 28d
      
      new_df <- get_z_results(emmeans, gLMM, title)
      table_df <- new_df
      table_df
      save_z_text(table_df)
      
    # forgotten memories #####
      # fit binomial generalized linear mixed model on forgotten sets
      gLMM <- glmer(forgotten ~ delay * emotion +
                                (1 | Name) + (1|set), data = transformationDf, family = "binomial", 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5))) 
      file_name = "gLMM_forgotten"
      result <- get_gLMM_results(gLMM=gLMM, title=file_name)
      create_gLMM_table(result, file_name = file_name)
      
      # post-hoc tests ####
      # paired
      title =  "difference between emotions at different delays forgotten"
      emmeans <- emmeans (gLMM, pairwise ~ emotion|delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      table_df <- get_z_results(emmeans = emmeans, gLMM = gLMM, title=title)
      table_df
      #neutral vs negative at 28d *
      
      # two-sample
      title <- "difference between between delays in different emotions forgotten"
      emmeans <- emmeans (gLMM, pairwise ~ delay|emotion, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
      table_df <- get_z_results(emmeans = emmeans, gLMM = gLMM, title=title)
      table_df
      #1d vs 28d in neutral ***   1d vs 28d in negative **
      
      #interaction contrast: 
      title <- "difference in delay-dependent increase between emotions forgotten"
      emmeans <- emmeans(gLMM, specs = ~ delay*emotion, lmer.df = "satterthwaite")
      
      neutral.1d = c(1, 0, 0, 0)
      neutral.28d = c(0, 1, 0, 0)
      negative.1d = c(0, 0, 1, 0)
      negative.28d = c(0, 0, 0, 1)
      
      stat <- pairs(contrast(emmeans, method = list("neutral 28d - neutral 1d" = neutral.28d - neutral.1d, 
                                            "negative 28d - negative 1d" = negative.28d - negative.1d)),adjust="sidak")
      summary(stat) #higher increase in forgotten items for neutral than for negative **
      

      # eff size for interaction contrast
      
      new_df <- compute_effect_size_z_interaction(stat, gLMM, title)
      table_df <- rbind(table_df, new_df)
      
      save_z_text(table_df)
      
    # semantically transformed memories #####
      #fit binomial generalized linear mixed model on semantically transformed memories
      gLMM <- glmer(semOnly_transformed ~ delay * emotion +
                                          (1 | Name) + (1|set), data = transformationDf, family = "binomial", 
                                        control=glmerControl(optimizer="bobyqa",
                                                             optCtrl=list(maxfun=2e5))) 

      file_name = "gLMM_semantically_transformed"
      result <- get_gLMM_results(gLMM=gLMM, title=file_name)
      create_gLMM_table(result, file_name = file_name)
      
      # post-hoc tests 
      title =  "1d vs 28d in sem"
      emmeans<- emmeans(gLMM, pairwise ~ delay, lmer.df = "satterthwaite")
      summary(emmeans) # 1d < 28d ***
      table_df <- get_z_results(emmeans=emmeans, gLMM=gLMM, title=title)
      table_df
      
      #interaction contrast: difference in delay-dependent increase between emotions
      title = "difference in delay-dependent increase between emotions in sem"
      emmeans<- emmeans(gLMM, specs = ~ delay*emotion, lmer.df = "satterthwaite")
      summary(emmeans)
      
      neutral.1d = c(1, 0, 0, 0)
      neutral.28d = c(0, 1, 0, 0)
      negative.1d = c(0, 0, 1, 0)
      negative.28d = c(0, 0, 0, 1)
      
      stat <- pairs(contrast(emmeans, method = list("neutral 28d - neutral 1d" = neutral.28d - neutral.1d, 
                                      "negative 28d - negative 1d" = negative.28d - negative.1d)),adjust="sidak")
      summary(stat)
      new_df <- compute_effect_size_z_interaction(stat, gLMM, title)
      table_df <- rbind(table_df, new_df)
      table_df
      
      # for plotting:
      # difference between emotions at different delays
      emmeansEmoDelay <- emmeans (semOnly_transformed_glmm, pairwise ~ emotion|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
      summary(emmeansEmoDelay, adjust="sidak") #sidak-adjustment if necessary #neutral vs negative n.s.
      eff_size(emmeansEmoDelay, sigma= sigma(semOnly_transformed_glmm), edf = Inf) 

      
      # difference between delays for different emotions
      title = "difference between delay in each emotion in sem"
      emmeans <- emmeans (gLMM, pairwise ~ delay|emotion, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
       #1d vs 28d in neutral* 1d vs 28d in negative ***
      new_df <- get_z_results(emmeans=emmeans, gLMM=gLMM, title=title)
      table_df <- rbind(table_df, new_df)
      table_df
      
      save_z_text(table_df)
      
    # perceptually transformed memories #####
      #fit binomial generalized linear mixed model on perceptually transformed memory 
      gLMM <- glmer(perOnly_transformed ~ delay * emotion +
                                          (1 | Name) + (1|set), data = transformationDf, family = "binomial", 
                                        control=glmerControl(optimizer="bobyqa",
                                                             optCtrl=list(maxfun=2e5))) 
      file_name = "gLMM_perceptually_transformed"
      result <- get_gLMM_results(gLMM=gLMM, title=file_name)
      create_gLMM_table(result, file_name = file_name)
      
  # write Supplementary table 4 into source file data ####
      subset_df <- transformationDf # before percentage was computed
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      
      # for supplementary table
      addWorksheet(wb, "SupplementaryTable4")
      writeData(wb, "SupplementaryTable4", subset_df)
      
  # Figure 2B ######
  # prepare data 
  # prepare for connected individual data points
   smaller_transformationDf[,c("detailed", "forgotten", 
                               "semOnly_transformed", 
                                "perOnly_transformed")] <- smaller_transformationDf[,c("detailed", 
                                                           "forgotten", "semOnly_transformed", 
                                                           "perOnly_transformed")] / 30 *100
      
  plot_data <- smaller_transformationDf %>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))
  # define dodge
  dodge = 0.15
  # plot mean perceptually transformed memory in percent 
  svg("Figure2B_perTransformed.svg")
  p <- ggplot (data=plot_data, aes(x=delay, y=perOnly_transformed, fill=emotion))+
  #facet_grid(~ emotion)+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), # new version (no more jittering)
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
  p + 
  coord_cartesian( ylim = c(0, 100)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  my_theme 
  
  # plot mean semantically transformed memory in percent   
  svg("Figure2B_semTransformed.svg")
  p <- ggplot (data=plot_data, aes(x=delay, y=semOnly_transformed, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
  p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  #1d vs 28d
  annotate(geom="text", x=1.5, y=0.9, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.03,xend=1.97,y=0.885,yend=0.885), size=1.5) + 
  #neut: 1d vs neg
  annotate(geom="text", x=1.25, y=0.8, label= c("*"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=0.79,xend=1.76,y=0.789,yend=0.789), size=1.5) +
  #neg: 1d vs 28d
  annotate(geom="text", x=1.75, y=0.7, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.29,xend=2.21,y=0.685,yend=0.685), size=1.5) +  
  #aestehtics
  coord_cartesian( ylim = c(0, 100)) +
  my_theme 
  
  dev.off()
  # plot mean forgotten memory in percent 
  svg("Figure2B_forgotten.svg")
  p <- ggplot (data=plot_data, aes(x=delay, y=forgotten, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), # new version (no more jittering)
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
  p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  
  annotate(geom="text", x=1.25, y=0.8, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=0.79,xend=1.76,y=0.789,yend=0.789), size=1.5) +
  #
  annotate(geom="text", x=2, y=0.6, label= c("*"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.79,xend=2.21,y=0.585,yend=0.585), size=1.5) +  
  #
  annotate(geom="text", x=1.75, y=0.7, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.29,xend=2.21,y=0.685,yend=0.685), size=1.5) +  
  #
  annotate(geom="text", x=1.5, y=0.9, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.03,xend=1.97,y=0.885,yend=0.885), size=1.5) + 
  coord_cartesian( ylim = c(0, 100)) +
  my_theme 
  dev.off()
  
  # plot mean detailed memory in percent 
  svg("Figure2B_detailled.svg")
  p <- ggplot (data=plot_data, aes(x=delay, y=detailed, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
  p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  #1d vs 28d
  annotate(geom="text", x=1.5, y=0.9, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.03,xend=1.97,y=0.885,yend=0.885), size=1.5) + 
  labs(y="memory specificity in delayed recognition (%)")+
  coord_cartesian( ylim = c(0, 1)) +
  my_theme 
  
  dev.off()

  # write into source_data ####
    # Write the data into sheet
    subset_df <- smaller_transformationDf %>% # before percentage was computed
      dplyr::rename(semantically_transformed = semOnly_transformed, perceptually_transformed = perOnly_transformed) %>%
      dplyr::select(Name, delay, emotion, perceptually_transformed, semantically_transformed, forgotten, detailed)
    subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
  
    # for supplementary table
      addWorksheet(wb, "SupplementaryTable3")
      writeData(wb, "SupplementaryTable3", subset_df)
  
    # for Figure 2B
      subset_df[,c("detailed", "forgotten", 
               "semantically_transformed", 
                "perceptually_transformed")] <- subset_df[,c("detailed", 
                                                             "forgotten", 
                                                             "semantically_transformed", 
                                                             "perceptually_transformed")] / 30 *100
      # add to source_data
      addWorksheet(wb, "Figure2B")
      writeData(wb, "Figure2B", subset_df)
  
# RELATEDNESS RATING ON DAY 3####
  # prepare data ####
    smaller_simRatingsDf <- subset(behavDf, itemType != "old") %>%
                            aggregate(cbind(percRating, semRating) ~ Name + delay + emotion + itemType, 
                                      FUN = mean) %>% 
                            mutate(lureType = factor(itemType))
    # over emotion and delay
    meanDf <- aggregate(cbind(percRating, semRating) ~ Name + lureType, 
                        FUN = function(x) mean(x, na.rm = TRUE), 
                        data = smaller_simRatingsDf)

  # analyze data #####
    # descriptive statistics for Supplementary Table 1 ####
      psych::describeBy(percRating ~ emotion + lureType + delay, data = smaller_simRatingsDf)
      psych::describeBy(semRating ~ emotion + lureType + delay, data = smaller_simRatingsDf)
      
      psych::describeBy(percRating ~ lureType, data = meanDf)
      psych::describeBy(semRating ~ lureType, data = meanDf)
    # Write to source data file ####
      subset_df <- smaller_simRatingsDf %>%
        dplyr::rename(perceptual_relatedness = percRating, semantic_relatedness = semRating) %>%
        dplyr::select(Name, delay, emotion, lureType, perceptual_relatedness, semantic_relatedness)
      # Define a named character vector of old and new level names
      level_names <- c("per" = "perceptually related", "sem" = "semantically related", "new" = "unrelated")
      # Use the plyr package's mapvalues function to rename levels
      subset_df$lureType <- plyr::mapvalues(subset_df$lureType, from = names(level_names), to = level_names)
      
      addWorksheet(wb, "SupplementaryTable5")
      writeData(wb, "SupplementaryTable5", subset_df)
      
     # analyze semantic relatedness rating #####
        # run ANOVA 
          ANOVA <- aov_ez(
            "Name"
            ,"semRating"
            ,smaller_simRatingsDf
            ,between=c("delay")
            ,within=c("emotion","lureType")
            ,anova_table="pes")
          print(ANOVA)
          summary(ANOVA)
          
          table_df <- get_ANOVA_results(ANOVA = ANOVA)
          save_ANOVA_text(table_df = table_df, title = "semantic relatedness rating")
      
        # paired
          title = "semantic relatedness rating between lures" 
          emmeans <- emmeans (ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite", adjust="sidak") # satterwhaite for fastening up computation, does not change results of contrasts
    
          table_df <- compute_effect_size_paired(emmeans, data = smaller_simRatingsDf, 
                                               within_variable = "lureType", 
                                               response_variable = "semRating", 
                                               group_variable = NULL)
          save_postHoc_t_text(header_row = title, table_df = table_df)
     
     # analyze perceptual relatedness rating ####
        # run ANOVA 
          ANOVA <- aov_ez(
            "Name"
            ,"percRating"
            ,smaller_simRatingsDf
            ,between=c("delay")
            ,within=c("emotion","lureType")
            ,anova_table="pes")
          
          table_df <- get_ANOVA_results(ANOVA)
          save_ANOVA_text(table_df, title = "perceptual relatedness rating")
        
        # post hoc tests 
          # paired
          title <- "difference in perceptual relatedness rating between lure"
          emmeans <- emmeans(ANOVA, pairwise ~ lureType, 
                             lmer.df = "satterthwaite", adjust="sidak") # satterwhaite for fastening up computation, does not change results of contrasts
          summary(emmeans) 
          
          new_df <- compute_effect_size_paired(emmeans = emmeans, data = smaller_simRatingsDf, within_variable = "lureType", response_variable = "percRating")

          table_df <- new_df # init
          table_df
          
          # two-sample t test
          title <- "difference between lure types within delay"
          emmeans <- emmeans(ANOVA, pairwise ~ delay|lureType, 
                                                 lmer.df = "satterthwaite", adjust="sidak") # satterwhaite for fastening up computation, does not change results of contrasts
          new_df <- compute_effect_size(emmeans = emmeans)
          table_df <- rbind(table_df, new_df)
          table_df
          # paired
          title <- "difference between luretypes in per rating within delay"
          emmeans <- emmeans(ANOVA, pairwise ~ lureType|delay, 
                                                 lmer.df = "satterthwaite", adjust="sidak") # satterwhaite for fastening up computation, does not change results of contrasts
          summary(perRating.emmeansDelayLure) # sidak adjustment when needed
          new_df <- compute_effect_size_paired(emmeans, data = smaller_simRatingsDf, 
                                               within_variable = "lureType", 
                                               response_variable = "percRating", 
                                               group_variable = "delay")
          table_df <- rbind(table_df, new_df)
          table_df
          
          header_row = "posthoc perceptual relatedness rating"
          save_postHoc_t_text(table_df = table_df, header_row = header_row)
    
    # difference between rating types in lures ####
       title = "difference in rating types in sem"
        ttest <- with(meanDf, t.test(semRating[lureType=="sem"], 
                            percRating[lureType=="sem"], 
                            paired = TRUE))
          # Convert to long format
          long_meanDf <- meanDf %>%
            filter(lureType == "sem") %>%
            pivot_longer(
              cols = c(percRating, semRating),
              names_to = "ratingType",
              values_to = "value"
            ) %>%
            mutate(ratingType = factor(ratingType))
          
        eff <- cohen.d(long_meanDf$value, long_meanDf$ratingType, alpha = 0.05)
        
        table_df <- get_manual_contrasts(ttest=ttest, eff=eff, title=title)
        
        unique(meanDf$lureType)
       
      # perceptually related lures
        title = "difference in rating types in per"
        ttest <- with(meanDf, t.test(semRating[lureType=="per"], 
                            percRating[lureType=="per"], 
                            paired = TRUE)) 
        # Convert to long format
        long_meanDf <- meanDf %>%
          filter(lureType == "per") %>%
          pivot_longer(
            cols = c(percRating, semRating),
            names_to = "ratingType",
            values_to = "value"
          ) %>%
          mutate(ratingType = factor(ratingType))
        
        eff <- cohen.d(long_meanDf$value, long_meanDf$ratingType, alpha = 0.05)
        
        new_df <- get_manual_contrasts(ttest=ttest, eff=eff, title=title)
        table_df <- rbind(table_df, new_df)
        

      # unrelated lures
        title = "difference in ratingTypes in new"
        ttest <- with(meanDf, t.test(x = semRating[lureType=="new"], 
                                          y = percRating[lureType=="new"], 
                                          paired=TRUE))
        long_meanDf <- meanDf %>%
          filter(lureType == "new") %>%
          pivot_longer(
            cols = c(percRating, semRating),
            names_to = "ratingType",
            values_to = "value"
          ) %>%
          mutate(ratingType = factor(ratingType))
        
        
        eff <- cohen.d(long_meanDf$value, long_meanDf$ratingType, alpha = 0.05)
        
        new_df <- get_manual_contrasts(ttest=ttest, eff=eff, title=title)
        table_df <- rbind(table_df, new_df)
        table_df
        
        save_postHoc_t_text(table_df=table_df, header_row = "differences in ratings within lures", round = FALSE)


    # check influence of outliers #####
      data <- smaller_simRatingsDf
      
      #semantic relatedness rating
  
        #neutral
        #1d - neutral - per 
        log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))))
        data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #1d - neutral - sem
        log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))))
        data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        #1d - neutral - new
        log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))))
        data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
        #no outlier
        
        #28d - neutral - per 
        log1 <- which(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))|  data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))))
        data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #28d - neutral - sem
        log1 <- which(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))))
        data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        #negative
        #1d - negative - per 
        log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))|  data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))))
        data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #1d - negative - sem
        log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))))
        data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        #1d - negative - new
        log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))|  data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))))
        data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
        #no outlier
        
        #28d - negative - per 
        log1 <- which(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))|  data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))))
        data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #28d - negative - sem
        log1 <- which(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))|  data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))))
        data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        
        #28d - negative - new
        log1 <- which(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))|  data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))))
        data$semRating[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
        #no outlier
        
        # no outliers!
      
      # perceptual relatedness rating
      
        #neutral
        #1d - neutral - per 
        log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))))
        data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #1d - neutral - sem
        log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))))
        data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        #1d - neutral - new
        log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))))
        data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
        #no outlier
        
        #28d - neutral - per 
        log1 <- which(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))|  data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))))
        data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #28d - neutral - sem
        log1 <- which(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))))
        data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        #negative
        #1d - negative - per 
        log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))|  data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))))
        data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #1d - negative - sem
        log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))))
        data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        #1d - negative - new
        log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))|  data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))))
        data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
        #no outlier
        
        #28d - negative - per 
        log1 <- which(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))|  data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))))
        data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
        #no outlier
        
        #28d - negative - sem
        log1 <- which(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))|  data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))))
        data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
        #no outlier
        
        #28d - negative - new
        log1 <- which(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))|  data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))))
        data$percRating[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
        #no outlier
        
        # no outliers!
        


  # Figure 3a ####
    # prepare data 
      # stack rating categories to the new variable ratingType with the value named rating
      plot_data <- meanDf %>% # make sure to run the new meanDf first
                    pivot_longer(cols = c("percRating", "semRating"),
                                  names_to = "ratingType", 
                                  values_to = "rating") %>%
                    mutate(lureType = factor(lureType, levels = c("per", "sem", "new"))) %>%
                    mutate(x = case_when(
                            lureType == "per" & ratingType == "percRating" ~ 1 - dodge,
                            lureType == "per" & ratingType == "semRating" ~ 1 + dodge,
                            lureType == "sem" & ratingType == "percRating" ~ 2 - dodge,
                            lureType == "sem" & ratingType == "semRating" ~ 2 + dodge,
                            lureType == "new" & ratingType == "percRating" ~ 3 - dodge,
                            lureType == "new" & ratingType == "semRating" ~ 3 + dodge,
                          ))
        
    # plot
      svg("Figure3a.svg")
      
      p <- ggplot (data=plot_data, aes(x=lureType, y=rating, fill=ratingType))+   
        stat_summary(fun='mean',geom='bar', position=position_dodge())+
        geom_point(pch = 19, position = position_dodge(0.6), 
                   alpha = 0.2, size=3)+
        geom_line(aes(x = x, group = interaction(Name, lureType)), alpha = 0.1) +
        labs(y="relatedness rating")
      p + theme_classic()+
        #annotation
        #percRel vs semRel in sem rating
        # stars 
        annotate("text", x = 1.75, y = 11, label = "* * *", size =15)+ 
        #line
        annotate("path", x = c(1.24, 2.21), y = c(10.8, 10.8), size=1.5) +
        #sem vs new in sem rating
        # stars 
        annotate("text", x = 2.75, y = 11, label = "* * *", size =15)+ 
        #line
        annotate("path", x = c(2.24, 3.21),  y = c(10.8, 10.8), size=1.5) +
        #sem vs perc rating in semLure
        # stars 
        annotate("text", x = 2, y = 9.9, label = "* * *", size =15)+ 
        #line
        annotate("path", x = c(0.79, 1.76), y = c(7.7, 7.7), size=1.5) +
        #line
        annotate("path", x = c(0.79, 1.21), y = c(6.6, 6.6), size=1.5) +
        # stars 
        annotate("text", x = 1, y = 6.8, label = "***", size = 15)+ 
        stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
        scale_fill_manual(values=c("lightsteelblue", "deepskyblue4"))+ labs(x="lure type")+
        scale_x_discrete(labels=c("perceptually \n related","semantically \n related","unrelated"))+
        coord_cartesian( ylim = c(0, 11)) + 
        scale_y_continuous(name ="relatedness rating", breaks=c(0, 2, 4, 6, 8, 10))+
        my_theme 
      
      dev.off()
      
  # Write Figure 3a to source data file ####
      
      subset_df <- meanDf %>%
        dplyr::rename(perceptual_relatedness = percRating, semantic_relatedness = semRating) 
      # Define a named character vector of old and new level names
      level_names <- c("per" = "perceptually related", "sem" = "semantically related", "new" = "unrelated")
      # Use the plyr package's mapvalues function to rename levels
      subset_df$lureType <- plyr::mapvalues(subset_df$lureType, from = names(level_names), to = level_names)
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      addWorksheet(wb, "Figure3a")
      writeData(wb, "Figure3a", subset_df)
  # Supplementary Figure 3 ####
  svg("SupplFig7_distributionOfRating.svg", height=10, width=5) 
    
  plot_data <- smaller_simRatingsDf %>%
    pivot_longer(cols = c("percRating", "semRating"),
                 names_to = "ratingType", 
                 values_to = "rating") %>%
    mutate(lureType = factor(lureType, levels=c('per','sem','new'), 
                             labels=c('perceptually related','semantically related','unrelated')))
    
  ggplot(plot_data, aes(x = rating, fill = ratingType, linetype=delay)) +
        facet_wrap(lureType ~ emotion, ncol=2)+
        geom_density(alpha = .4, size=1) +
        scale_linetype_manual(values=c("solid", "dashed"))+
        scale_fill_manual(labels = c('perceptual','semantical'), 
                          values=c("lightsteelblue", "deepskyblue4"))+ 
        labs(x = "relatedness rating", y = "density") +
        theme_classic()+
        theme(panel.spacing.x = unit(2, "lines"),
              axis.title.y=element_text(size=16, 
                                        family="Encode Sans Condensed"),
              axis.text.y = element_text(size = 12, colour="black", 
                                         family = "Encode Sans Condensed"),
              axis.title.x=element_text(size=16, family = "Encode Sans Condensed"),
              axis.text.x = element_text(size = 12, colour="black", 
                                         family="Encode Sans Condensed"),
              legend.title = element_blank(),
              legend.text = element_text(size = 16, 
                                         family = "Encode Sans Condensed"),
              legend.position = "top",
              axis.line =  element_line(size=1.75),
              axis.ticks = element_line(size=1.75, colour="black"),
              axis.ticks.length = unit(.2,"cm"),
              strip.text = element_text(size=16, 
                                        family = "Encode Sans Condensed"),
              strip.background=element_rect(color="white"))
    
  dev.off()
  
  # Write Supplementary Figure 3 to source data file ####
    subset_df <- smaller_simRatingsDf %>%
      dplyr::rename(perceptual_relatedness = percRating, semantic_relatedness = semRating) %>%
      dplyr::select(Name, delay, emotion, lureType, perceptual_relatedness, semantic_relatedness)
    # Define a named character vector of old and new level names
    level_names <- c("per" = "perceptually related", "sem" = "semantically related", "new" = "unrelated")
    # Use the plyr package's mapvalues function to rename levels
    subset_df$lureType <- plyr::mapvalues(subset_df$lureType, from = names(level_names), to = level_names)
    subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
    
    addWorksheet(wb, "SupplementaryFigure3")
    writeData(wb, "SupplementaryFigure3", subset_df)
# INCREASE IN PROBABILITY FOR A FALSE ALARM ON DAY 2 DEPENDING ON RELATEDNESS RATING ON DAY 3 ####
  # prepare data ####
    # long file for LMM
      simRatingsDf <- subset(behavDf, itemType != "old") %>% 
                            select("Name", "set", "stimulusTypeNum", "delay", "emotion", 
                                       "itemType", "FA", "semRating", "percRating") %>% 
                            mutate(lureType = factor(itemType, levels=c("new","sem","per")))

    # group mean centering ratings

      simRatingsDf_1d <- simRatingsDf %>% 
        filter(delay == '1d')
      
      simRatingsDf_1d$semRatingGroupMeanCent <- scale(simRatingsDf_1d$semRating, scale=FALSE)
      simRatingsDf_1d$percRatingGroupMeanCent <- scale(simRatingsDf_1d$percRating, scale=FALSE)
      
      simRatingsDf_28d <- simRatingsDf %>% 
        filter(delay == '28d')
      
      simRatingsDf_28d$semRatingGroupMeanCent <- scale(simRatingsDf_28d$semRating, scale=FALSE)
      simRatingsDf_28d$percRatingGroupMeanCent <- scale(simRatingsDf_28d$percRating, scale=FALSE)
      
      simRatingsDf <- rbind(simRatingsDf_1d, simRatingsDf_28d)

  # analyze data ####
    # fit binomial generalized linear mixed model with group mean centered ratings 
      gLMM <- glmer(FA ~ delay*semRatingGroupMeanCent*percRatingGroupMeanCent*emotion + 
                      (1 | Name) + (1 | stimulusTypeNum), data = simRatingsDf, family = "binomial",
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5))) 
      
      file_name <- "gLMM_FA_relatedness"
      result <-  get_gLMM_results(gLMM = gLMM, title = title)
      ft <- create_gLMM_table(result=result, file_name = file_name)
    
    # FOR COMPARISON: fit binomial generalized linear mixed model with grand mean centered ratings 
      # grand mean centering ratings 
      simRatingsDf$semRatingGrandMeanCent <- scale(simRatingsDf$semRating, scale=FALSE)
      simRatingsDf$percRatingGrandMeanCent <- scale(simRatingsDf$percRating, scale=FALSE)
      
      # fit gLMMM
      glmm <- glmer(FA ~ delay*semRatingGrandMeanCent*percRatingGrandMeanCent*emotion + 
                      (1 | Name) + (1 | stimulusTypeNum), data = simRatingsDf, family = "binomial",
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5))) 
      summary(glmm) # no change
      
  # Figure 3b #####
    # perceptual relatedness 
 
      svg("Figure3b_perceptualRelatedness.svg")
      
      p <- plot_model(glmm_groupMeanCent, type = "pred", terms = c("percRatingGroupMeanCent","emotion","delay"),
                      show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                      dot.size = 4, grid = FALSE, line.size = 2, 
                      axis.title = c("perceptual relatedness","% false alarms in delayed recognition"), #set x-and y-axis title
                      colors = c("azure4", "firebrick4")) + 
        p + 
        coord_cartesian( ylim = c(0, 0.16)) +
       my_theme
        
      dev.off()
      
    # semantic relatedness 
    
      svg("Figure3b_semanticRelatedness.svg")
        
      p <- plot_model(glmm_groupMeanCent, type = "pred", terms = c("semRatingGroupMeanCent","emotion","delay"), 
                      dot.size = 4, grid = FALSE, line.size = 3, 
                      axis.title = c("semantic relatedness","probability of a false alarm in %"), #set x-and y-axis title
                      colors = c("azure4", "firebrick4")) 
      p + 
        coord_cartesian( ylim = c(0, 0.16)) +
        my_theme
      
      dev.off()
    
  # Write Figure 3 and Supplementary Table 6 into source_data ####
    subset_df <- simRatingsDf %>%
      dplyr::rename(perceptual_relatedness_centered = percRatingGroupMeanCent, semantic_relatedness_centered = semRatingGroupMeanCent) %>%
      dplyr::select(Name, delay, emotion, stimulusTypeNum, perceptual_relatedness_centered, semantic_relatedness_centered)
 
    subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
    
    addWorksheet(wb, "Figure3b_SupplementaryTable6")
    writeData(wb, "Figure3b_SupplementaryTable6", subset_df)
    
# FA FOR SEMANTICALLY RELATED LURES LOW VS HIGH IN SEMANTIC RELATEDNESS ####
  #median split
  median <- median(simRatingsDf$percRating) #compute median of perceptual relatedness rating
  median
      
  simRatingsDf$percGroup <- simRatingsDf$percRating #new variable for median split the perceptual relatedness rating
  simRatingsDf$percGroup <- ifelse(simRatingsDf$percGroup <= median, 'lowpercRating', 'highpercRating') #group perceptual relatedness rating depending whether low/equal or higher than median

  # prepare file for glmm
  semOnlyDf <- subset(simRatingsDf, lureType == "sem") %>%  # only semantically related lures
              dplyr::select(Name, delay, semRating, emotion, stimulusTypeNum, FA, percGroup) %>%
              mutate(percGroup = factor(percGroup, levels=c("lowpercRating","highpercRating")))
    
  # run glmm
  gLMM <- glmer(FA ~ percGroup*delay*emotion + 
                  (1 | Name) + (1 | stimulusTypeNum), 
                data = semOnlyDf, 
                family = "binomial",
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) 
  summary(gLMM) #no difference of high or low perceptual relatedness on the probability for a false alarm
  
  file_name = "gLMM_sem_lowVsHighInPercRel"
  result <-  get_gLMM_results(gLMM = gLMM, title = file_name)
  ft <- create_gLMM_table(result=result, file_name = file_name)
  
  # add Supplementary Table 7 to Source Data file ##### 
    subset_df <- semOnlyDf %>%
      dplyr::select(Name, delay, emotion, stimulusTypeNum, FA, percGroup)
    
    subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
    
    addWorksheet(wb, "SupplementaryTable7")
    writeData(wb, "SupplementaryTable7", subset_df)
# MODEL-BASED REPRESENTATIONAL-SIMILARITY ANALYSIS####
  # HIPPOCAMPAL LONG AXIS ####
    # LEFT HIPPOCAMPUS ######
      # prepare data ####
        longaxisL_modelRSADf <- modelRSADf %>% 
                                dplyr::select(Name, model, emotion, delay, 
                                              anteriorHC_L, posteriorHC_L) %>%
                                pivot_longer(cols = c(anteriorHC_L, posteriorHC_L),
                                             names_to = "longaxis",
                                             values_to = "fit") %>%
                                mutate(longaxis = factor(longaxis))
    
      # analyze data ####

        #run ANOVA 
          results.ANOVA <- aov_ez(
            "Name"
            ,"fit"
            ,longaxisL_modelRSADf
            ,between=c("delay")
            ,within=c("emotion","model","longaxis")
            ,anova_table="pes")
          print(results.ANOVA) ### --> trend for delay x model x longaxis interaction, sign. delay x model x long axis x emotion interaction
          summary(results.ANOVA)
        
        # post hoc tests 
          title = "anterior vs posterior separately for both groups"
          emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*longaxis, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
          summary(emmeans) #adjustment if necessary
          
          # significant decrease in fit from 1d to 28d in anterior HC (after exclusion from one outlier) in model 1
          # significant decrease in fit from 1d to 28d in anterior HC (after exclusion from one outlier) in model 2
          
          title = "anterior vs posterior separately for both groups"
          emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*emotion*longaxis, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
          summary(.emmeans) #adjustment if necessary
          
          # trend p = 0.760 in model 2 (1d-28d, aHC) that disappears after outlier exclusion, i.e. is driven by one outlier
      
      # test influence of outliers #####
        # look for outliers ####
          data <- modelRSADf %>% 
            group_by(Name) %>% 
            dplyr::select(Name, model, emotion, delay, anteriorHC_L, posteriorHC_L) 
          
          #anterior HC
          #model 1
          log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #1
          
          log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
          data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
          data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #1 outlier 3L
          
          #model 2
          log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
          data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
          data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          # # 1 outlier 
          
          #model 3
          log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none 
          
          log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
          data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
          data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #1 outlier 3L
          
          #posterior HC
          #model 1
          log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
          data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
          data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #1 outlier
          
          #model 2
          log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #1 outlier 
          
          log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
          data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
          data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          #none
          
          #model 3
          log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
          data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
          data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
      
        # prepare data ####
          # remove outlier 
            log = data$anteriorHC_L == 999
            data$anteriorHC_L[log] = NA
            
            log = data$posteriorHC_L == 999
            data$posteriorHC_L[log] = NA
        
          # prepare data without outlier 
            longaxisL_modelRSADf <- data %>% 
                                    dplyr::select(Name, model, emotion, 
                                                  delay, anteriorHC_L, posteriorHC_L) %>%
                                    pivot_longer(cols = c(anteriorHC_L, posteriorHC_L),
                                                 names_to = "longaxis",
                                                 values_to = "fit") %>%
                                    mutate(longaxis = factor(longaxis))
      
        # analyze data ####
          # run ANOVA before outlier exclusion
            ANOVA <- aov_ez(
              "Name"
              ,"fit"
              ,longaxisL_modelRSADf
              ,between=c("delay")
              ,within=c("emotion","model","longaxis")
              ,anova_table="pes")
            print(ANOVA) # -> not consistent with previous data (outlier sj07 biased results, especially for model 2) -> outlier corrected data further used
            summary(ANOVA)
            
            header_row <- "model comparison RSA in HC"
            table_df <- get_ANOVA_results(ANOVA = ANOVA)
            save_ANOVA_text(table_df = table_df, title = header_row)
        
        # post hoc tests
          # two_sample
            title = "delay effect separately for model and longaxis"
            emmeans <- emmeans (ANOVA,pairwise ~ delay|model*longaxis, adjust="sidak",
                                    lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
            summary(emmeans) #adjustment if necessary
            
            new_df <- compute_effect_size(emmeans = emmeans, model = ANOVA)
            new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
            table_df <- new_df
        
        # Figure 4b ####
          #drop outlier for whole plot as it is also dropped for the ANOVA 
          #don't run this line if you want to see data with outlier
          data<-data[!(data$Name=="sj07"),] 
          
          my_y_title <- expression(paste("Fisher transformed", italic("rho")))
          
          # model 1
          plot_data <- subset(data, model == "model1") %>%
            mutate(x = case_when(
              delay == "1d" & emotion == "neutral" ~ 1 - dodge,
              delay == "1d" & emotion == "negative" ~ 1 + dodge,
              delay == "28d" & emotion == "neutral" ~ 2 - dodge,
              delay == "28d" & emotion == "negative" ~ 2 + dodge,
            ))
          
          svg("Figure4B_laHC_model1.svg")
          
          p <- ggplot (data=plot_data, aes(x=delay, y=anteriorHC_L, fill=emotion))+
            facet_grid(~ model)+
            stat_summary(fun='mean',geom='bar', position=position_dodge())+
            geom_point(pch = 19, position = position_dodge(0.6), 
                       alpha = 0.2, size=3)+
            geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
          p + 
            stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
            scale_x_discrete(labels=c("1d", "28d"))+
            scale_fill_manual(values=c("azure4", "firebrick4"))+
            annotate(geom="text", x=1.5, y=0.052, label= c("*"), color="black", size = 15) +
            geom_segment(aes(x=1,xend=2,y=0.046,yend=0.046), size=1.5) +
            labs(y=my_y_title)+ coord_cartesian( ylim =c(-0.06, 0.10)) +
            my_theme
          
          dev.off()
          
          svg("Figure4B_lpHC_model1.svg")
          
          p <- ggplot (data=plot_data, aes(x=delay, y=posteriorHC_L, fill=emotion))+
            facet_grid(~ model)+
            stat_summary(fun='mean',geom='bar', position=position_dodge())+
            geom_point(pch = 19, position = position_dodge(0.6), 
                       alpha = 0.2, size=3)+
            geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.2) 
          p + 
            stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
            scale_x_discrete(labels=c("1d", "28d"))+
            scale_fill_manual(values=c("azure4", "firebrick4"))+
            labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.06, 0.10)) +
            my_theme
          
          dev.off()
          
          # model 2
          plot_data <- subset(data, model == "model2") %>%
            mutate(x = case_when(
              delay == "1d" & emotion == "neutral" ~ 1 - dodge,
              delay == "1d" & emotion == "negative" ~ 1 + dodge,
              delay == "28d" & emotion == "neutral" ~ 2 - dodge,
              delay == "28d" & emotion == "negative" ~ 2 + dodge,
            ))
          
          svg("Figure4B_laHC_model2.svg")
          
          p <- ggplot (data=plot_data, aes(x=delay, y=anteriorHC_L, fill=emotion))+
            facet_grid(~ model)+
            stat_summary(fun='mean',geom='bar', position=position_dodge())+
            geom_point(pch = 19, position = position_dodge(0.6), 
                       alpha = 0.2, size=3)+
            geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
          p + 
            stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
            scale_x_discrete(labels=c("1d", "28d"))+
            scale_fill_manual(values=c("azure4", "firebrick4"))+
            labs(y=my_y_title)+ coord_cartesian(ylim =c(-0.06, 0.10)) +
            my_theme
          
          dev.off()
          
          svg("Figure4B_lpHC_model2.svg")
          
          p <- ggplot (data=plot_data, aes(x=delay, y=posteriorHC_L, fill=emotion))+
            facet_grid(~ model)+
            stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
            geom_point(pch = 19, position = position_dodge(0.6), 
                       alpha = 0.2, size=3)+
            geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
          p + 
            stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
            scale_x_discrete(labels=c("1d", "28d"))+
            scale_fill_manual(values=c("azure4", "firebrick4"))+
            labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.06, 0.10)) +
            my_theme
          
          dev.off()
          
          # model 3
          plot_data <- subset(data, model == "model3") %>%
            mutate(x = case_when(
              delay == "1d" & emotion == "neutral" ~ 1 - dodge,
              delay == "1d" & emotion == "negative" ~ 1 + dodge,
              delay == "28d" & emotion == "neutral" ~ 2 - dodge,
              delay == "28d" & emotion == "negative" ~ 2 + dodge,
            ))
          
          svg("Figure4B_laHC_model3.svg")
          
          p <- ggplot (data=plot_data, aes(x=delay, y=anteriorHC_L, fill=emotion))+
            facet_grid(~ model)+
            stat_summary(fun='mean',geom='bar', position=position_dodge())+
            geom_point(pch = 19, position = position_dodge(0.6), 
                       alpha = 0.2, size=3)+
            geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
          p + 
            stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
            scale_x_discrete(labels=c("1d", "28d"))+
            scale_fill_manual(values=c("azure4", "firebrick4"))+
            annotate(geom="text", x=1.5, y=0.052, label= c("* *"), color="black", size = 15) +
            geom_segment(aes(x=1,xend=2,y=0.046,yend=0.046), size=1.5) +
            labs(y=my_y_title)+ coord_cartesian( ylim =c(-0.06, 0.10)) +
            theme_classic()+
            my_theme
          
          dev.off()
          
          svg("Figure4B_lpHC_model3.svg")
          
          p <- ggplot (data=plot_data, aes(x=delay, y=posteriorHC_L, fill=emotion))+
            facet_grid(~ model)+
            stat_summary(fun='mean',geom='bar', position=position_dodge())+
            geom_point(pch = 19, position = position_dodge(0.6), 
                       alpha = 0.2, size=3)+
            geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
          p + 
            stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
            scale_x_discrete(labels=c("1d", "28d"))+
            scale_fill_manual(values=c("azure4", "firebrick4"))+
            labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.06, 0.10)) +
            my_theme
          
          dev.off()
      # save Figure 4b to source_data #####
        subset_df <- longaxisL_modelRSADf[!(longaxisL_modelRSADf$Name=="sj07"),] 
          
        # Define a named character vector of old and new level names
        level_names <- c("anteriorHC_L" = "anterior_hippocampus", "posteriorHC_L" = "posterior_hippocampus")
        # Use the plyr package's mapvalues function to rename levels
        subset_df$longaxis <- plyr::mapvalues(subset_df$longaxis, from = names(level_names), to = level_names)
        
        subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
        
        
        addWorksheet(wb, "Figure4B")
        writeData(wb, "Figure4B", subset_df)
        
    # RIGHT HIPPOCAMPUS ####
      # prepare data ####
        longaxisR_modelRSADf <- modelRSADf %>% 
                                dplyr::select(Name, model, emotion, delay, 
                                              anteriorHC_R, posteriorHC_R) %>%
                                pivot_longer(cols = c(anteriorHC_R, posteriorHC_R),
                                             names_to = "longaxis",
                                             values_to = "fit") %>%
                                mutate(longaxis = factor(longaxis))
      
      # analyze data ####
        # run ANOVA 
          results.ANOVA <- aov_ez(
            "Name"
            ,"fit"
            ,longaxisR_modelRSADf
            ,between=c("delay")
            ,within=c("emotion","model","longaxis")
            ,anova_table="pes")
          print(results.ANOVA)
          summary(results.ANOVA)
        # post hoc tests 
          #anterior vs posterior separately for both groups
          rHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*emotion*longaxis, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
          summary(rHC.emmeans, adjusts="sidak") #adjustment if necessary
          
      # check influence of outliers #####
        # look for outliers ####
          data <- modelRSADf
          
          #anterior HC
          #model 1
          log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
          data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
          data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #3
          
          #model 2
          log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
          data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
          data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          # 1 outlier
          
          #model 3
          log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          # 1 outlier
          
          log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
          data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
          data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
          
          #posterior HC
          #model 1
          log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
          data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
          data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #3
          
          #model 2
          log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
          data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
          data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          #none
          
          #model 3
          log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
          data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
          data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
          
        # prepare data ####
          # drop outlier
            # removing sj07 and sj10
            log = data$anteriorHC_R == 999
            data$anteriorHC_R[log] = NA
            
            log = data$posteriorHC_R == 999
            data$posteriorHC_R[log] = NA
            
          # prepare data without outliers 
            longaxisR_modelRSADf <- data %>% 
                                    dplyr::select(Name, model, emotion, 
                                                  delay, anteriorHC_R, posteriorHC_R) %>%
                                    pivot_longer(cols = c(anteriorHC_R, posteriorHC_R),
                                                 names_to = "longaxis",
                                                 values_to = "fit") %>%
                                    mutate(longaxis = factor(longaxis))
                              
        # analyze data ####
          # run ANOVA without outliers
            results.ANOVA <- aov_ez(
              "Name"
              ,"fit"
              ,longaxisR_modelRSADf
              ,between=c("delay")
              ,within=c("emotion","model","longaxis")
              ,anova_table="pes")
            print(results.ANOVA) # removing 2 outliers (sj07 and sj10) 
            summary(results.ANOVA)
      
          # post hoc tests after removing outliers
            #anterior vs posterior separately for both groups
            rHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*emotion*longaxis, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
            summary(rHC.emmeans, adjusts="sidak") #adjustment if necessary
            eff_size(rHC.emmeans, sigma = Inf, edf = 50) 

  # NEOCORTEX#####
    # neocortical memory ROI #####
      # analyze data ####
        # ANOVA 
          ANOVA <- aov_ez(
            "Name"
            ,"neocorticalMemoryROI"
            ,modelRSADf
            ,between=c("delay")
            ,within=c("emotion","model")
            ,anova_table="pes")
          print(ANOVA)
          summary(ANOVA)
          
          header_row = "model RSA in neocortex"
          table_df <- get_ANOVA_results(ANOVA = ANOVA)
          save_ANOVA_text(table_df = table_df, title = header_row)
        
        # post hoc tests 
          # two sample
          title = "delay effect within each model"
          emmeans <- emmeans (ANOVA, pairwise ~ delay|model, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
          summary(emmeans) #adjustment if necessary
          
          new_df <- compute_effect_size(emmeans = emmeans, model = ANOVA)
          new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
          table_df <- new_df
          
          # paired 
          title = "difference between models within each delay"
          emmeans <- emmeans (ANOVA, pairwise ~ model|delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
          summary(emmeans) #adjustment if necessary
          

          new_df <- compute_effect_size_paired(emmeans = emmeans, data = modelRSADf, group_variable = "delay", 
                                       within_variable = "model", response_variable = "neocorticalMemoryROI")
          new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
          table_df <- rbind(table_df, new_df)
          
          save_postHoc_t_text(table_df = table_df, start_col = 3, header_row = header_row)
          
          #prepare Df
          meanDf <- aggregate(neocorticalMemoryROI ~ Name + delay + model, FUN = mean, data = modelRSADf)
        
      # check influence of outliers #####
        # look for outliers 
          
          data <- modelRSADf
          
          #model 1
          log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
          data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
          data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
          data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
          #none
          
          #model 2
          log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
          data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
          data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
          data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
          # none
          
          #model 3
          log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none 
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
          data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
          data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
          
          log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
          data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
          #none
      
          # no outlier!
          
      # Figure 5 upper panel #####
      
        my_y_title <- expression(paste("Fisher transformed", italic(" rho")))
        
        svg("Figure5_neocort_model1.svg")
        
        plot_data <- behavDf_model1 %>%
          mutate(x = case_when(
            delay == "1d" & emotion == "neutral" ~ 1 - dodge,
            delay == "1d" & emotion == "negative" ~ 1 + dodge,
            delay == "28d" & emotion == "neutral" ~ 2 - dodge,
            delay == "28d" & emotion == "negative" ~ 2 + dodge,
          ))
        
        p <- ggplot (data=plot_data, aes(x=delay, y=neocorticalMemoryROI, fill=emotion))+
          facet_grid(~ model)+
          stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          
          labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
        
        dev.off()
        
        
        svg("Figure5_neocort_model2.svg")
        
        plot_data <- behavDf_model2 %>%
          mutate(x = case_when(
            delay == "1d" & emotion == "neutral" ~ 1 - dodge,
            delay == "1d" & emotion == "negative" ~ 1 + dodge,
            delay == "28d" & emotion == "neutral" ~ 2 - dodge,
            delay == "28d" & emotion == "negative" ~ 2 + dodge,
          ))
        
        p <- ggplot (data=plot_data, aes(x=delay, y=neocorticalMemoryROI, fill=emotion))+
          facet_grid(~ model)+
          stat_summary(stat = 'identity', fun='mean',geom='bar', 
                       position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          # delay effect
          annotate(geom="text", x=1.5, y=0.11, label= c("*"), color="black",  size = 15) +
          geom_segment(aes(x=1,xend=2,y=0.1,yend=0.1), size=1.5) +
          
          labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
          
        dev.off()
      
        
        svg("Figure5_neocort_model3.svg")
        
        plot_data <- behavDf_model3 %>%
                      mutate(x = case_when(
                        delay == "1d" & emotion == "neutral" ~ 1 - dodge,
                        delay == "1d" & emotion == "negative" ~ 1 + dodge,
                        delay == "28d" & emotion == "neutral" ~ 2 - dodge,
                        delay == "28d" & emotion == "negative" ~ 2 + dodge,
                      ))
        
        p <- ggplot (data=plot_data, aes(x=delay, y=neocorticalMemoryROI, 
                                         fill=emotion))+
          facet_grid(~ model)+
          stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),
                       width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          
          labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
        
        dev.off()
      
      # save to source_data ####
        subset_df <- modelRSADf[, 2:6] %>% # copy second to third column into new df
        dplyr::rename(fit_in_neocorticalMemoryROI = neocorticalMemoryROI) 
        subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
        
        addWorksheet(wb, "Figure5_upperPanel")
        writeData(wb, "Figure5_upperPanel", subset_df)
    # individual ROIs ####
      # vmPFC ####
        ANOVA <- aov_ez(
          "Name"
          ,"vmPFC"
          ,subset(modelRSADf, model == "model2")
          ,between=c("delay")
          ,within=c("emotion")
          ,anova_table="pes")
        print(ANOVA)
        summary(ANOVA)
        
        header_row = "model 2 in vmPFC"
        table_df <- get_ANOVA_results(ANOVA = ANOVA)
        save_ANOVA_text(table_df = table_df, title = header_row)
      
        # post hoc tests
        # two-sample
        title = "delay effect in model 2"
        emmeans <- emmeans (ANOVA, pairwise ~ delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans) #adjustment if necessary
        
        new_df <- compute_effect_size(emmeans = emmeans, model = ANOVA)
        new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
        table_df <- new_df
        
        save_postHoc_t_text(table_df = table_df, start_col = 3, header_row = header_row)
        
      # IFG####
        ANOVA <- aov_ez(
          "Name"
          ,"IFG"
          ,subset(modelRSADf, model == "model2")
          ,between=c("delay")
          ,within=c("emotion")
          ,anova_table="pes")
        print(ANOVA)
        summary(ANOVA)
        
        header_row = "model 2 in IFG"
        table_df <- get_ANOVA_results(ANOVA = ANOVA)
        save_ANOVA_text(table_df = table_df, title = header_row)
        
      # aCC ####
        ANOVA <- aov_ez(
          "Name"
          ,"aCC"
          ,subset(modelRSADf, model == "model2")
          ,between=c("delay")
          ,within=c("emotion")
          ,anova_table="pes")
        print(ANOVA)
        summary(ANOVA)
        
        header_row = "model 2 in aCC"
        table_df <- get_ANOVA_results(ANOVA = ANOVA)
        save_ANOVA_text(table_df = table_df, title = header_row)
        
      # Angular Gyrus####
        # right #####
          ANOVA <- aov_ez(
            "Name"
            ,"angularGyrus_R"
            ,subset(modelRSADf, model == "model2")
            ,between=c("delay")
            ,within=c("emotion")
            ,anova_table="pes")
          print(ANOVA)
          summary(ANOVA)
          
          header_row = "model 2 in AG R"
          table_df <- get_ANOVA_results(ANOVA = ANOVA)
          save_ANOVA_text(table_df = table_df, title = header_row)
        
          # posthoc
          # two-sample
          titel = "delay effect in model 2 in AG R"
          emmeans <- emmeans(ANOVA, pairwise ~ delay, adjust="sidak",
                                             lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
          summary(emmeans) #adjustment if necessary
          
          new_df <- compute_effect_size(emmeans = emmeans, model = ANOVA)
          new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
          table_df <- new_df
          
          save_postHoc_t_text(table_df = table_df, start_col = 3, header_row = header_row)

        # left ####
          ANOVA <- aov_ez(
            "Name"
            ,"angularGyrus_L"
            ,subset(modelRSADf, model == "model2")
            ,between=c("delay")
            ,within=c("emotion")
            ,anova_table="pes")
          print(ANOVA)
          summary(ANOVA)
          
          header_row = "model 2 in AG L"
          table_df <- get_ANOVA_results(ANOVA = ANOVA)
          save_ANOVA_text(table_df = table_df, title = header_row)
          
      # Precuneus #####
          ANOVA <- aov_ez(
            "Name"
            ,"Precuneus"
            ,subset(modelRSADf, model == "model2")
            ,between=c("delay")
            ,within=c("emotion")
            ,anova_table="pes")
          print(ANOVA)
          summary(ANOVA)
          
          header_row = "model 2 in Precuneus"
          table_df <- get_ANOVA_results(ANOVA = ANOVA)
          save_ANOVA_text(table_df = table_df, title = header_row)
          
          # posthoc
          # two-sample
          title <- "delay effect in model 2 in Prec"
          emmeans <- emmeans (ANOVA, pairwise ~ delay, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
          summary(emmeans) #adjustment if necessary
          
          new_df <- compute_effect_size(emmeans = emmeans, model = ANOVA)
          new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
          table_df <- new_df
          
          save_postHoc_t_text(table_df = table_df, start_col = 3, header_row = header_row)
        
      # Figure 5 lower panel #####
        plot_data <- subset(modelRSADf, model == "model2") %>%
          mutate(x = case_when(
            delay == "1d" & emotion == "neutral" ~ 1 - dodge,
            delay == "1d" & emotion == "negative" ~ 1 + dodge,
            delay == "28d" & emotion == "neutral" ~ 2 - dodge,
            delay == "28d" & emotion == "negative" ~ 2 + dodge,
          ))
        
        svg("Figure5_Precuneus.svg")
        
        p <- ggplot (data=plot_data, aes(x=delay, y=Precuneus, fill=emotion))+
          stat_summary(fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          annotate(geom="text", x=1.5, y=0.165, label= c("+"), color="black", size = 15) +
          geom_segment(aes(x=1,xend=2,y=0.15,yend=0.15), size=1.5) +
          labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
        
        dev.off()
        
        
        svg("Figure5_AGR.svg")
        
        p <- ggplot (data=plot_data, aes(x=delay, y=angularGyrus_R, fill=emotion))+
          stat_summary(fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          annotate(geom="text", x=1.5, y=0.165, label= c("*"), color="black", 
                   fontface = "bold", size = 15) +
          geom_segment(aes(x=1,xend=2,y=0.15,yend=0.15), size=1.5) +
          labs(y=my_y_title)+coord_cartesian( ylim = c(-0.1, 0.3)) +
          theme_classic()+
          my_theme
        
        dev.off()
        
        svg("Figure5_AGL.svg")
        
        p <- ggplot (data=plot_data, aes(x=delay, y=angularGyrus_R, fill=emotion))+
          stat_summary(fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          labs(y=my_y_title)+coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
        
        dev.off()
        
        svg("Figure5_vmPFC.svg")
        
        p <- ggplot (data=plot_data, aes(x=delay, y=vmPFC, fill=emotion))+
          stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          annotate(geom="text", x=1.5, y=0.165, label= c("*"), color="black", 
                   fontface = "bold", size = 15) +
          geom_segment(aes(x=1,xend=2,y=0.15,yend=0.15), size=1.5) +
          labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
        
        dev.off()
        
        
        svg("Figure5_IFG.svg")
        p <- ggplot (data=plot_data, aes(x=delay, y=IFG, fill=emotion))+
          stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
        
        dev.off()
        
        
        svg("Figure5_aCC.svg")
        
        p <- ggplot (data=plot_data, aes(x=delay, y=aCC, fill=emotion))+
          stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
          geom_point(pch = 19, position = position_dodge(0.6), 
                     alpha = 0.2, size=3)+
          geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
        p + 
          stat_summary(fun.data = mean_se, geom = "errorbar",  
                       position=position_dodge(0.9),width = 0, size = 1.7)+
          scale_x_discrete(labels=c("1d", "28d"))+
          scale_fill_manual(values=c("azure4", "firebrick4"))+
          labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
          my_theme
        
        dev.off()
        
      # save to source_data ####
        subset_df <- subset(modelRSADf, model == "model2") %>% 
                    dplyr::select(Name, model, emotion, delay, 
                                  vmPFC, aCC, IFG, Precuneus, angularGyrus_L, angularGyrus_R) %>%
                    pivot_longer(cols = c(vmPFC, aCC, IFG, Precuneus, angularGyrus_L, angularGyrus_R),
                                        names_to = "ROI",
                                  values_to = "fit") 
        subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
        
        
        addWorksheet(wb, "Figure5_lowerPanel")
        writeData(wb, "Figure5_lowerPanel", subset_df)
        
    # sensory control ROIs #####
      # occipital pole ####
        ANOVA <- aov_ez(
          "Name"
          ,"occPole"
          ,modelRSADf
          ,between=c("delay")
          ,within=c("emotion","model")
          ,anova_table="pes")
        print(ANOVA)
        summary(ANOVA)
        
        header_row = "model 2 in occPole"
        table_df <- get_ANOVA_results(ANOVA = ANOVA)
        save_ANOVA_text(table_df = table_df, title = header_row)
        
        # posthoc 
        title = "delay effect"
        emmeans <- emmeans (ANOVA, pairwise ~ model, lmer.df = "satterthwaite", adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans) #adjustment if necessary
        
        new_df <- compute_effect_size(emmeans = emmeans, model = ANOVA)
        new_df <- data.frame(title = rep(title, nrow(new_df)), new_df)
        table_df <- new_df
        save_postHoc_t_text(table_df = table_df, start_col = 3, header_row = header_row)

      # Heschl's gyrus ####
        ANOVA <- aov_ez(
          "Name"
          ,"HeschlGyrus"
          ,modelRSADf
          ,between=c("delay")
          ,within=c("emotion","model")
          ,anova_table="pes")
        print(ANOVA)
        summary(ANOVA)
        
        header_row = "model 2 in heschl"
        table_df <- get_ANOVA_results(ANOVA = ANOVA)
        save_ANOVA_text(table_df = table_df, title = header_row)
      
# MEMORY REINSTATEMENT OVER TIME #####
  # HIPPOCAMPAL LONGAXIS #####  
    # left hippocampus ####
      # prepare data ####
        # for analysis of delay-dependent change along the hippocampus
          longaxisL_ERSDf <- subset(reinstatementDf, itemType == 'old' & EncRuns == 'AllEncRuns') %>%
                              select(Name, set, delay, emotion, 
                                     anteriorHC_L,posteriorHC_L)%>%
                              dplyr::rename(anterior = anteriorHC_L, posterior = posteriorHC_L)%>%
                              pivot_longer(cols=c(anterior,posterior),
                                           names_to="longaxis",
                                           values_to = "ERS")%>%
                              mutate(longaxis = factor(longaxis))
        
        # for analyses dependening on behavior  
          behavDf$reactionTime <- behavDf$reactionTimeLastAnswer
          
          smallerBehavDf <- subset(behavDf, itemType == "old", 
                                   select = c("Name", "set", "delay", "emotion", 
                                              "detailed", "miss", "sem_FA", "per_FA","hit",
                                              "reactionTime"))%>%
                            mutate(detailed = factor(detailed),
                                   hit = factor(hit),
                                   sem_FA = factor(sem_FA),
                                   per_FA = factor(per_FA))
          
          mergedERSDf <- merge(longaxisL_ERSDf, 
                               smallerBehavDf, by = c("Name","delay","emotion","set")) 
          
      # change in memory reinstatement over time ####
        # analyze data ####
          # run linear mixed model
            LMM <- lmer(ERS ~ delay*emotion*longaxis +
                              (1 | Name) + (1 | set), 
                            data = longaxisL_ERSDf)
          
            file_name = "LMM_ERS_lHC"
            result <- get_LMM_results(LMM = LMM, Bonf = TRUE)
            ft <- create_LMM_table(result=result, file_name = file_name)
            
            # two-sample
            emmeans <- emmeans (LMM, pairwise ~ delay | longaxis, lmer.df = "satterthwaite", lmerTest.limit = 6240, adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
            eff <- eff_size(emmeans, sigma=sigma(LMM), edf = 122)

            table_df <- get_postHoc_LMM(emmeans, eff)
            print(table_df)
            
          # paired
            emmeans <- emmeans (LMM, pairwise ~ longaxis | delay, lmer.df = "satterthwaite", lmerTest.limit = 6240, adjust="sidak") #satterwhaite for fastening up computation, does not change results of contrasts
            eff <- eff_size(emmeans, sigma=sigma(LMM), edf = 122)
            
            table_df <- rbind(table_df, get_postHoc_LMM(emmeans, eff))
            print(table_df)
            
            save_postHoc_t_text(table_df = table_df, header_row = "ERS in lHC", round = FALSE)
            
          # add Supplementary Table 8 to Source Data file  
            subset_df <- longaxisL_ERSDf
            subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
            
            addWorksheet(wb, "SupplementaryTable8")
            writeData(wb, "SupplementaryTable8", subset_df)
            
          # hits only
            hitsLongaxisL_ERSDf <- subset(mergedERSDf, hit == "1")
            
            LMM <- lmer(ERS ~ delay*emotion*longaxis +
                              (1 | Name) + (1 | set), data = hitsLongaxisL_ERSDf)
            summary(LMM)
            
            file_name = "LMM_ERS_hitONLY_lHC"
            result <- get_LMM_results(LMM = LMM, Bonf = TRUE)
            ft <- create_LMM_table(result=result, file_name = file_name)
            
            # add Supplementary Table 9 to Source Data file  
            subset_df <- hitsLongaxisL_ERSDf
            subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
            
            addWorksheet(wb, "SupplementaryTable9")
            writeData(wb, "SupplementaryTable9", subset_df)
            

        # Figure 6 right upper panel #####
          # anterior hippocampus 
            svg("Figure6Upper_leftAnteriorHC.svg")
            
            plot_data <- subset(longaxisL_ERSDf, longaxis == "anterior")%>%
                          aggregate(ERS ~ Name + delay + emotion, 
                                    FUN = mean) %>%
                          mutate(x = case_when(
                            delay == "1d" & emotion == "neutral" ~ 1 - dodge,
                            delay == "1d" & emotion == "negative" ~ 1 + dodge,
                            delay == "28d" & emotion == "neutral" ~ 2 - dodge,
                            delay == "28d" & emotion == "negative" ~ 2 + dodge,
                          ))
            
            p <- ggplot (data=plot_data, aes(x=delay, y=ERS, fill=emotion))+
              stat_summary(fun='mean',geom='bar', position=position_dodge())+
              geom_point(pch = 19, position = position_dodge(0.6), 
                         alpha = 0.2, size=3)+
              geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
            p + 
              stat_summary(fun.data = mean_se, geom = "errorbar",  
                           position=position_dodge(0.9),width = 0, size = 1.7)+
              scale_x_discrete(labels=c("1d", "28d"))+
              scale_fill_manual(values=c("azure4", "firebrick4"))+
              labs(y="Fisher transformed r")+ coord_cartesian( ylim = c(-0.02, 0.05)) +
              my_theme
            
            dev.off()
          
          # posterior hippocampus
            svg("Figure6Upper_leftPosteriorHC.svg")
            
            plot_data <- subset(longaxisL_ERSDf, longaxis == "posterior")%>%
                          aggregate(ERS ~ Name + delay + emotion, 
                                    FUN = mean) %>%
                          mutate(x = case_when(
                            delay == "1d" & emotion == "neutral" ~ 1 - dodge,
                            delay == "1d" & emotion == "negative" ~ 1 + dodge,
                            delay == "28d" & emotion == "neutral" ~ 2 - dodge,
                            delay == "28d" & emotion == "negative" ~ 2 + dodge,
                          ))
            
            p <- ggplot (data=plot_data, aes(x=delay, y=ERS, fill=emotion))+
              stat_summary(fun='mean',geom='bar', position=position_dodge())+
              geom_point(pch = 19, position = position_dodge(0.6), 
                         alpha = 0.2, size=3)+
              geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
            p + 
              stat_summary(fun.data = mean_se, geom = "errorbar",  
                           position=position_dodge(0.9),width = 0, size = 1.7)+
              scale_x_discrete(labels=c("1d", "28d"))+
              scale_fill_manual(values=c("azure4", "firebrick4"))+
              labs(y="Fisher transformed r")+ coord_cartesian( ylim = c(-0.02, 0.05)) +
              annotate(geom="text", x=1.5, y=0.11, label= c("* *"), 
                       color="black", fontface = "bold", size = 15) +
              geom_segment(aes(x=1,xend=2,y=0.1,yend=0.1), size=1.5) +
              my_theme
            
            dev.off()
          
        # add Figure 6 upper panel to source data file ####
          subset_df <- longaxisL_ERSDf %>%
              aggregate(ERS ~ Name + delay + emotion + longaxis, 
                        FUN = mean)
          
          # rename levels to make it consistent with previous sheet 
          level_names <- c("anterior" = "anterior_hippocampus", "posterior" = "posterior_hippocampus")
          # Use the plyr package's mapvalues function to rename levels
          subset_df$longaxis <- plyr::mapvalues(subset_df$longaxis, from = names(level_names), to = level_names)
          subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
          
            
          addWorksheet(wb, "Figure6_upperPanel")
          writeData(wb, "Figure6_upperPanel", subset_df)
          
      # association of remote posterior ERS with memory semantization #####
        # prepare data ####
          postHCERSDf <- subset(mergedERSDf, longaxis == "posterior")
            
        # analyze data ####
          # semantically related lures 
          gLMM <- glmer(sem_FA ~ ERS*emotion*delay +
                                (1| Name)+(1|set), data =  postHCERSDf, family = "binomial", 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5))) 
          summary(gLMM)
          
          file_name = "gLMM_ERS_semFA_lpHC"
          title = "ERS"
          
          result <- get_gLMM_results(gLMM = gLMM, title = title)
          ft <- create_gLMM_table(result=result, file_name = file_name)
          
          
          # perceptually related lures 
          gLMM <- glmer(per_FA ~ ERS*emotion*delay +
                                (1| Name)+(1|set), data =  postHCERSDf, 
                              family = "binomial", 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5))) 
          summary(gLMM)
        
          file_name = "gLMM_ERS_perFA_lpHC"
          title = "ERS"
          result <- get_gLMM_results(gLMM = gLMM, title = title)
          ft <- create_gLMM_table(result=result, file_name = file_name)
          
          
        # Figure 6 lower right panel ####
          # semantically related lures
            svg("Figure6lower_semanticallyRelated.svg")
            
            p <- plot_model(glmm_semFA, type = "eff", terms = c("ERS","delay"), 
                            show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                            dot.size = 4, grid = FALSE, line.size = 3, 
                            axis.title = c("ERS in Fisher transformed r",
                                           "probability for a false alarm in %"), #set x-and y-axis title
                            colors = c("deepskyblue4", "deepskyblue4"))  
            p + 
              coord_cartesian( ylim = c(0, 0.50)) +
              my_theme
            
            dev.off()
        
          # perceptually related lures
            svg("Figure6lower_perceptuallyRelated.svg")
            
            p <- plot_model(glmm_perFA, type = "eff", terms = c("ERS","delay"), 
                            show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                            dot.size = 4, grid = FALSE, line.size = 3, 
                            axis.title = c("ERS in Fisher transformed r",
                                           "probability for a false alarm in %"), #set x-and y-axis title
                            colors = c("lightsteelblue", "lightsteelblue"))  
            p + 
              coord_cartesian( ylim = c(0, 0.50)) +
              my_theme
            
            dev.off()
        
        # add Figure 6 and Supplementary Table 12 to source data file ####
            subset_df <- postHCERSDf %>%
              dplyr::select(Name, emotion, delay, set, ERS, sem_FA, per_FA) %>%
              dplyr::rename(posteriorHippocampal_ERS = ERS) 
            subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
            

            addWorksheet(wb, "Figure6_SupplementaryTable10")
            writeData(wb, "Figure6_SupplementaryTable10", subset_df)
            
        # additional analyses ####
          # hits ####
            gLMM <- glmer(hit ~ ERS*emotion*delay +
                                (1| Name)+(1|set), data = postHCERSDf, 
                              family = "binomial", 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5))) 
            summary(gLMM)
            
            file_name = "gLMM_ERS_hits_lpHC"
            title = "ERS"
            result <- get_gLMM_results(gLMM = gLMM, title = title)
            ft <- create_gLMM_table(result=result, file_name = file_name)
            
            header_row = "pHC ERS and hits"
            save_gLMM_text(table_df = table_df, title = header_row)
            
            # add Supplementary 10 to source data file #### #####
            subset_df <- postHCERSDf %>%
              dplyr::select(Name, emotion, delay, set, ERS, hit) %>%
              dplyr::rename(posteriorHippocampal_ERS = ERS) 
            subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
            
            addWorksheet(wb, "SupplementaryTable10")
            writeData(wb, "SupplementaryTable10", subset_df)
        
          # detailed ####  
            gLMM <- glmer(detailed ~ ERS*emotion*delay +
                                     (1| Name)+(1|set), data = postHCERSDf, 
                                   family = "binomial", 
                                   control=glmerControl(optimizer="bobyqa",
                                                        optCtrl=list(maxfun=2e5))) 
            summary(gLMM)
            
            file_name = "gLMM_ERS_detailed_lpHC"
            result <- get_gLMM_results(gLMM = gLMM, title = title)
            ft <- create_gLMM_table(result=result, file_name = file_name)
            
            save_gLMM_text(table_df = table_df, title = header_row)
          
            # Supplementary Figure 4 
            svg("S4_ERSandDetailedMemory.svg", height = 8, width = 12)
            
            p <- plot_model(glmm_detailed, type = "pred", terms = c("ERS[all]","emotion","delay"), 
                            show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                            dot.size = 4, grid = FALSE, line.size = 3, 
                            axis.title = c("ERS in Fisher transformed r",
                                           "probability of detailed memory in %"), #set x-and y-axis title
                            colors = (values=c("azure4", "firebrick4")))  
            p + 
              coord_cartesian( ylim = c(0, 1)) + 
              ggtitle("posterior hippocampal ERS and detailed memory")+
              my_theme
            
            dev.off()
            # add Supplementary Figure 4 and Supplementary Table 11 to source data file ####
            subset_df <- postHCERSDf %>%
              dplyr::select(Name, emotion, delay, set, ERS, detailed) %>%
              dplyr::rename(posteriorHippocampal_ERS = ERS) 
            subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
            
            
            addWorksheet(wb, "SupplFig4_SupplTab11")
            writeData(wb, "SupplFig4_SupplTab11", subset_df)
          
          # reaction time
            LMM <- lmer(reactionTime ~ ERS*delay*emotion +
                          (1 | Name)+(1|set), data = postHCERSDf)
            summary(LMM)
            
            file_name = "LMM_ERS_RT_lpHC"
            result <- get_LMM_results(LMM = LMM)
            ft <- create_LMM_table(result=result, file_name = file_name)
        
         # Supplementary Table 13 to source data file ####
            subset_df <- postHCERSDf %>%
              dplyr::select(Name, emotion, delay, set, ERS, reactionTime) %>%
              dplyr::rename(posteriorHippocampal_ERS = ERS) 
            subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
            
            addWorksheet(wb, "SupplementaryTable13")
            writeData(wb, "SupplementaryTable13", subset_df)
            
    # right hippocampus #####
      # prepare data ####
        longaxisR_ERSDf <- subset(reinstatementDf, itemType == 'old' & EncRuns == 'AllEncRuns') %>%
                            select(Name, set, delay, emotion, 
                                   anteriorHC_R,posteriorHC_R)%>%
                            dplyr::rename(anterior = anteriorHC_R, posterior = posteriorHC_R)%>%
                            pivot_longer(cols=c(anterior,posterior),
                                         names_to="longaxis",
                                         values_to = "ERS")%>%
                            mutate(longaxis = factor(longaxis))
      # analyze data ####  
        LMM <- lmer(ERS ~ delay*emotion*longaxis +
                      (1 | set), data = longaxisR_ERSDf)
        summary(LMM)#
  
# EXPLORE REINSTATEMENT BY RELATED STIMULI ######
  # reinstatement by SEMANTICALLY related stimuli ####    
    # prepare data ####
      # long-file with longaxis as factor 
        longaxisL_EncSemSimDf <-  subset(reinstatementDf, itemType == 'sem' & EncRuns == 'AllEncRuns') %>%
                                  select(Name, set, delay, emotion, 
                                         anteriorHC_L,posteriorHC_L)%>%
                                  dplyr::rename(anterior = anteriorHC_L, posterior = posteriorHC_L)%>%
                                  pivot_longer(cols=c(anterior,posterior),
                                               names_to="longaxis",
                                               values_to = "EncSemSim")%>%
                                  mutate(longaxis = factor(longaxis))
        
    # analyze data ####
      # run LMM    
        LMM <- lmer(EncSemSim ~ delay*emotion*longaxis +
                      (1 | Name) + (1 | set), data = longaxisL_EncSemSimDf)
        summary(LMM)#
        
        file_name = "LMM_EncSemSim_lHC"
        result <- get_LMM_results(LMM = LMM)
        ft <- create_LMM_table(result=result, file_name = file_name)
        
      # post hoc tests ####
        emmeans <- emmeans (LMM,pairwise ~ longaxis, lmerTest.limit = 6240, adjusts="sidak",
                                lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans) #adjustment if necessary
        
      #  eff <- eff_size(emmeans, sigma=sigma(LMM), edf=?)
  
    # Supplementary Figure 5 ####

      svg("S5_semReinstatement.svg", width=10, height=5)
        
      plot_data <- longaxisL_EncSemSimDf %>%
        aggregate(EncSemSim ~ Name + delay + emotion + longaxis, 
                  FUN = mean) %>%
        mutate(x = case_when(
          delay == "1d" & emotion == "neutral" ~ 1 - dodge,
          delay == "1d" & emotion == "negative" ~ 1 + dodge,
          delay == "28d" & emotion == "neutral" ~ 2 - dodge,
          delay == "28d" & emotion == "negative" ~ 2 + dodge,
        ))
    
      p <- ggplot (data=plot_data, aes(x=delay, y=EncSemSim, fill=emotion))+
        facet_grid(~ longaxis)+
        stat_summary(fun='mean',geom='bar', position=position_dodge())+
        geom_point(pch = 19, position = position_dodge(0.6), 
                   alpha = 0.2, size=3)+
        geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
      p + 
        stat_summary(fun.data = mean_se, geom = "errorbar",  
                     position=position_dodge(0.9),width = 0, size = 1.7)+
        scale_x_discrete(labels=c("1d", "28d"))+
        scale_fill_manual(values=c("azure4", "firebrick4"))+
        labs(y="Fisher transformed r")+ coord_cartesian( ylim = c(-0.02, 0.05)) +
        my_theme
      
      dev.off()
  
    # add to source data file ####
      subset_df <- longaxisL_EncSemSimDf %>%
        aggregate(EncSemSim ~ Name + delay + emotion + longaxis, FUN = mean) %>%
        dplyr::rename(reinstatementBySemanticallyRelatedLures = EncSemSim) 
      
      # rename levels to make it consistent with previous sheets
      level_names <- c("anterior" = "anterior_hippocampus", "posterior" = "posterior_hippocampus")
      # Use the plyr package's mapvalues function to rename levels
      subset_df$longaxis <- plyr::mapvalues(subset_df$longaxis, from = names(level_names), to = level_names)
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      
      addWorksheet(wb, "SupplementaryFigure5")
      writeData(wb, "SupplementaryFigure5", subset_df)
      
  # reinstatement by PERCEPTUALLY related stimuli ####
    # prepare data ####
      longaxisL_EncPercSimDf <-  subset(reinstatementDf, itemType == 'per' & EncRuns == 'AllEncRuns') %>%
                                  select(Name, set, delay, emotion, 
                                         anteriorHC_L,posteriorHC_L)%>%
                                  dplyr::rename(anterior = anteriorHC_L, posterior = posteriorHC_L)%>%
                                  pivot_longer(cols=c(anterior,posterior),
                                               names_to="longaxis",
                                               values_to = "EncPercSim")%>%
                                  mutate(longaxis = factor(longaxis))
      
    # analyze data ####
      LMM <- lmer(EncPercSim ~ delay*emotion*longaxis +
                    (1 | Name), data = longaxisL_EncPercSimDf)
      summary(LMM)
      
      file_name = "LMM_EncPercSim_lHC"
      result <- get_LMM_results(LMM = LMM)
      ft <- create_LMM_table(result=result, file_name = file_name)
      
      # add Supplementary 14 to source data file ####
      subset_df <- postHCERSDf %>%
        dplyr::select(Name, emotion, delay, set, ERS, sem_FA, per_FA) %>%
        dplyr::rename(posteriorHippocampal_ERS = ERS) 
      subset_df <- merge(longaxisL_EncPercSimDf, longaxisL_EncSemSimDf, by = c("Name","set","delay","emotion","longaxis"))
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      
      addWorksheet(wb, "SupplementaryTable14")
      writeData(wb, "SupplementaryTable14", subset_df)
    
# EXPLORE REINSTATEMENT IN THE NEOCORTEX ####
  # ERS ####
    ERSDf <-subset(reinstatementDf, itemType == 'old' & EncRuns == 'AllEncRuns')
  
    LMM <- lmer(neocorticalMemoryROI ~ delay*emotion +
                  (1 | Name)+(1|set), data = ERSDf)

    file_name = "LMM_ERS_neocorticalMemoryROI"
    result <- get_LMM_results(LMM = LMM)
    ft <- create_LMM_table(result=result, file_name = file_name)
    
    LMM <- lmer(occPole ~ delay*emotion +
                  (1 | Name)+(1|set), data = ERSDf)
    
    file_name = "LMM_ERS_occPole"
    result <- get_LMM_results(LMM = LMM)
    ft <- create_LMM_table(result=result, file_name = file_name)
    
    LMM <- lmer(HeschlGyrus ~ delay*emotion +
                  (1 | Name)+(1|set), data = ERSDf)
    
    file_name = "LMM_ERS_HeschlGyrus"
    result <- get_LMM_results(LMM = LMM)
    ft <- create_LMM_table(result=result, file_name = file_name)
  
  # add Supplementary Table 15 to source data file ####    
    subset_df <- ERSDf %>%
      dplyr::select(Name, emotion, delay, set, neocorticalMemoryROI, occPole, HeschlGyrus) 
    subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
    
    addWorksheet(wb, "SupplementaryTable15")
    writeData(wb, "SupplementaryTable15", subset_df)
    
  # reinstatement by related lures #####
    
  # reinstatement by semantically related lures ####
    # prepare data ####
      EncSemSimDf <-  subset(reinstatementDf, itemType == 'sem' & EncRuns == 'AllEncRuns')
    # run analysis ####  
      LMM <- lmer(neocorticalMemoryROI ~ delay*emotion +
                    (1 | Name)+(1|set), data = EncSemSimDf)
      
      file_name = "LMM_EncSemSim_neocorticalMemoryROI"
      result <- get_LMM_results(LMM = LMM)
      ft <- create_LMM_table(result=result, file_name = file_name)
    
      LMM <- lmer(occPole ~ delay*emotion +
                    (1 | Name)+(1|set), data = EncSemSimDf)
      
      file_name = "LMM_EncSemSim_occ"
      result <- get_LMM_results(LMM = LMM)
      ft <- create_LMM_table(result=result, file_name = file_name)
      
      LMM <- lmer(HeschlGyrus ~ delay*emotion +
                    (1 | Name)+(1|set), data = EncSemSimDf)
      summary(LMM)
      file_name = "LMM_EncSemSim_Hescl"
      result <- get_LMM_results(LMM = LMM)
      ft <- create_LMM_table(result=result, file_name = file_name)
      
    # add Supplementary Table 16 to source data file ####    
      subset_df <- EncSemSimDf %>%
        dplyr::select(Name, emotion, delay, set, neocorticalMemoryROI, occPole, HeschlGyrus) 
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      
      addWorksheet(wb, "SupplementaryTable16")
      writeData(wb, "SupplementaryTable16", subset_df)

  # reinstatement by perceptually related lures ####
    # prepare data ####  
      EncPercSimDf <-  subset(reinstatementDf, itemType == 'per' & EncRuns == 'AllEncRuns')
    # run analysis ####  
      LMM <- lmer(neocorticalMemoryROI ~ delay*emotion +
                    (1 | Name)+(1|set), data = EncPercSimDf)
      summary(LMM)#
      
      file_name = "LMM_EncPerSim_neoc"
      result <- get_LMM_results(LMM = LMM)
      ft <- create_LMM_table(result=result, file_name = file_name)
      
      LMM <- lmer(occPole ~ delay*emotion +
                    (1 | Name)+(1|set), data = EncPercSimDf)
      summary(LMM)#
      
      file_name = "LMM_EncPerSim_occ"
      result <- get_LMM_results(LMM = LMM)
      ft <- create_LMM_table(result=result, file_name = file_name)
      
      LMM <- lmer(HeschlGyrus ~ delay*emotion +
                    (1 | Name), data = EncPercSimDf)
      summary(LMM)#
      
      file_name = "LMM_EncPerSim_heschl"
      result <- get_LMM_results(LMM = LMM)
      ft <- create_LMM_table(result=result, file_name = file_name)
      
   # add Supplementary Table 17 to source data file ####    
      subset_df <- EncPercSimDf %>%
        dplyr::select(Name, emotion, delay, set, neocorticalMemoryROI, occPole, HeschlGyrus) 
      subset_df <- merge(subset_df, controlDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      
      addWorksheet(wb, "SupplementaryTable17")
      writeData(wb, "SupplementaryTable17", subset_df)
    
# VALENCE & AROUSAL RATING####
  # prepare data ####
    emoRatingsDf <- aggregate(cbind(arousalRating, valenceRating) ~ Name + delay + emotion, 
                        FUN = mean, data = behavDf) %>%
                    mutate(delay = factor(delay),
                           emotion = factor(emotion))
        
  # analyze data ####
    
    psych::describeBy(cbind(arousalRating, valenceRating) ~ emotion, data = meanDf)
    
    # arousal  
    title <- "arousal Rating"
    ttest <- with(emoRatingsDf, t.test(arousalRating ~ emotion, paired = TRUE))
    eff <- cohen.d(emoRatingsDf$arousalRating, emoRatingsDf$emotion, alpha = 0.05)
    table_df <- get_manual_contrasts(ttest=ttest, eff=eff, title=title)
    
    # valence
    title <- "valence Rating"
    ttest <- with(emoRatingsDf, t.test(valenceRating ~ emotion, paired = TRUE))
    eff <- cohen.d(emoRatingsDf$valenceRating, emoRatingsDf$emotion, alpha = 0.05)
    table_df <- get_manual_contrasts(ttest=ttest, eff=eff, title=title)
    table_df

# DIFFERENCE BETWEEN D2 AND D3 IN DELAY BETWEEN GROUPS ####
    describe(controlDf$delayD3)
    
    title <- "difference between delay groups in day3 timing"
    ttest <- with(controlDf, t.test(delayD3 ~ delay))
    eff <- cohen.d(controlDf$delayD3, controlDf$delay, alpha = 0.05)
    table_df <- get_manual_contrasts(ttest=ttest, eff=eff, title=title)
    table_df
    
# QUESTIONNAIRES ####

    #STAI - state
    title <- "delay in stai state"
    ttest <- with(controlDf, t.test(STAI_State_SUM ~ delay))
    eff <-  cohen.d(controlDf$STAI_State_SUM, controlDf$delay, alpha = 0.05)
    table_df <- get_manual_contrasts(ttest=ttest, eff=eff, title=title)
    
    #STAI- trait
    title <- "delay in stai trait"
    ttest <- with(controlDf, t.test(STAI_Trait_SUM ~ delay))
    eff <-  cohen.d(controlDf$STAI_Trait_SUM, controlDf$delay, alpha = 0.05)
    table_df <- rbind(table_df, get_manual_contrasts(ttest=ttest, eff=eff, title=title))
    
    #PSQI
    
    #28d
    title <- "delay in psqi 28d"
    ttest <- with(controlDf, t.test(PSQI_28d_SUM ~ delay))
    eff <-  cohen.d(controlDf$PSQI_28d_SUM, controlDf$delay, alpha = 0.05)
    table_df <- rbind(table_df, get_manual_contrasts(ttest=ttest, eff=eff, title=title))
    
    psych::describeBy(controlDf$PSQI_28d_SUM , group = controlDf$delay)
    
    #24h - sleep quality
    title <- "24h sleep quality"
    ttest <- with(controlDf, t.test(PSQI_24h_6 ~ delay))
    eff <-  cohen.d(controlDf$PSQI_24h_6, controlDf$delay, alpha = 0.05)
    table_df <- rbind(table_df, get_manual_contrasts(ttest=ttest, eff=eff, title=title))
    
    #24h - sleep duration
    title <- "PSQI 24h sleep duration"
    ttest <- with(controlDf, t.test(PSQI_24h_4 ~ delay))
    eff <-  cohen.d(controlDf$PSQI_24h_4, controlDf$delay, alpha = 0.05)
    table_df <- rbind(table_df, get_manual_contrasts(ttest=ttest, eff=eff, title=title))
    
    psych::describeBy(controlDf$PSQI_24h_4 , group = controlDf$delay)
    
    #BDI
    title <- "difference between delay groups in BDI"
    ttest <- with(controlDf, t.test(BDI_SUM ~ delay))
    eff <-  cohen.d(controlDf$BDI_SUM, controlDf$delay, alpha = 0.05)
    table_df <- rbind(table_df, get_manual_contrasts(ttest=ttest, eff=eff, title=title))
    
    #TICS
    title <-  "difference between delay groups in TICS"
    ttest <- with(controlDf, t.test(TICS_SSCS ~ delay))
    eff <-  cohen.d(controlDf$TICS_SSCS, controlDf$delay, alpha = 0.05)
    table_df <- rbind(table_df, get_manual_contrasts(ttest=ttest, eff=eff, title=title))
    
    save_postHoc_t_text(table_df = table_df, header_row ="questionnaire data")
    
    
    psych::describeBy(controlDf$PSQI_24h_6 , group = controlDf$delay)
    
    
    
    # write Supplementary Table to source data file ####
    # add to source data file #
    subset_df <- controlDf %>%
      dplyr::select(Name, delay, STAI_State_SUM, STAI_Trait_SUM,
                    PSQI_28d_SUM, PSQI_24h_4, PSQI_24h_6, 
                    BDI_SUM, TICS_SSCS, sex) %>%
      dplyr::rename(state_anxiety = STAI_State_SUM,
                    trait_anxiety = STAI_Trait_SUM,
                    PSQI_globalScore_28d = PSQI_28d_SUM, 
                    PSQI_sleepQuality_24h = PSQI_24h_4, 
                    PSQI_sleepLatency_24h = PSQI_24h_6,
                    depressiveMood = BDI_SUM, 
                    subjectiveChronicStress = TICS_SSCS) 
    
    addWorksheet(wb, "SupplementaryTable18")
    writeData(wb, "SupplementaryTable18", subset_df)
    
# BEHAVIORAL PILOT####
  # sociodemographics ####
  # before exclusion/ dropout
    describe(pilotDemoBeforeExclusionDf$age)
    as.data.frame(table(pilotDemoBeforeExclusionDf$sex))
  
  # final sample
    pilotFinalDemoDf <- subset(pilotDemoBeforeExclusionDf, Name != 'pilot_62') ##drop participant who didn't finish task
    describe(pilotFinalDemoDf$age)
    as.data.frame(table(pilotFinalDemoDf$sex))
  
  # results for all pilot sets####
    # prepare data 
      perRating_PilotDf  <- aggregate(rating ~ Name + lureType + emotion, 
                                      FUN = mean, data = subset(pilotAllSetsDf, 
                                                                ratingScale == "Per"))
      semRating_PilotDf  <- aggregate(rating ~ Name + lureType + emotion, 
                                      FUN = mean, data = subset(pilotAllSetsDf, 
                                                                ratingScale == "Sem"))
    # analyze data 
      # perceptual relatedness Rating
        # run ANOVA 
        ANOVA <- aov_ez(
          "Name"
          ,"rating"
          ,perRating_PilotDf
          ,within=c("emotion","lureType")
          ,anova_table="pes")
        print(ANOVA)
        summary(ANOVA)
        
        row_header = "per rating in pilot ANOVA"
        table_df <- get_ANOVA_results(ANOVA = ANOVA)
        save_ANOVA_text(table_df = table_df, title = row_header)
        # post hoc tests 
        title = "difference in perceptual relatedness rating between lures"
        emmeans <- emmeans (ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite", 
                            adjust="sidak") # satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans) # sidak adjustment when needed
        table_df <- compute_effect_size_paired(emmeans = emmeans, data = perRating_PilotDf, 
                                   within_variable = "lureType", response_variable = "rating")
        save_postHoc_t_text(header_row = title, 
                            table_df = table_df, start_col = 2)
      
      # semantic relatedness Rating 
        # run ANOVA 
          ANOVA <- aov_ez(
            "Name"
            ,"rating"
            ,semRating_PilotDf
            ,within=c("emotion","lureType")
            ,anova_table="pes")
          print(ANOVA)
          summary(ANOVA)
          
          row_header = "sem rating in pilot ANOVA"
          table_df <- get_ANOVA_results(ANOVA = ANOVA)
          save_ANOVA_text(table_df = table_df, title = row_header)
    
        # post hoc tests
          title = "difference in semantic relatedness rating between lures"
          emmeans <- emmeans (ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
          summary(emmeans, adjust="sidak") # sidak adjustment when needed
          
          table_df <- compute_effect_size_paired(emmeans = emmeans, data = perRating_PilotDf, 
                                                 within_variable = "lureType", response_variable = "rating")
          save_postHoc_t_text(header_row = title, 
                              table_df = table_df, start_col = 2)
    
  # results of final sets ####
    # descriptive statistics
      meanDf <- aggregate(rating ~ Name + lureType + ratingScale, FUN = mean, 
                          data = pilotFinalSetsDf)    
          
      psych::describeBy(rating ~ lureType + ratingScale, data = meanDf)
      
  # semantic relatedness Rating ####
    # run ANOVA
      ANOVA <- aov_ez(
        "Name"
        ,"rating"
        ,subset(pilotFinalSetsDf, ratingScale == "semantic relatedness")
        ,within=c("emotion","lureType")
        ,anova_table="pes")
      print(ANOVA)
      summary(ANOVA)
      
      row_header = "sem rating in pilot final set ANOVA"
      
      table_df <- get_ANOVA_results(ANOVA = ANOVA)
      save_ANOVA_text(table_df = table_df, title = row_header)
      
      # post hoc tests
      emmeans <- emmeans (ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
      summary(emmeans, adjust="sidak") # sidak adjustment when needed
      data = subset(pilotFinalSetsDf, ratingScale == "semantic relatedness") %>%
        mutate(lureType = gsub(" ", ".", lureType)) # change levels of lure type to how emmeans renames them
      
      table_df <- compute_effect_size_paired(emmeans = emmeans, data = data, 
                                             within_variable = "lureType", response_variable = "rating")
      save_postHoc_t_text(header_row = row_header, 
                          table_df = table_df, start_col = 2)
  
    # perceptual relatedness Rating####
      # run ANOVA 
        ANOVA <- aov_ez(
          "Name"
          ,"rating"
          ,subset(pilotFinalSetsDf, ratingScale == "perceptual relatedness")
          ,within=c("emotion","lureType")
          ,anova_table="pes")
        print(ANOVA)
        summary(ANOVA)
        
        row_header = "per rating in pilot final set ANOVA"
        table_df <- get_ANOVA_results(ANOVA = ANOVA)
        save_ANOVA_text(table_df = table_df, title = row_header)
        
      # post hoc tests 
        title = "difference in perceptual relatedness rating between lures"
        emmeans <- emmeans (ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite", adjust="sidak") # satterwhaite for fastening up computation, does not change results of contrasts
        summary(emmeans) # sidak adjustment when needed
        data = subset(pilotFinalSetsDf, ratingScale == "perceptual relatedness") %>%
          mutate(lureType = gsub(" ", ".", lureType)) # change levels of lure type to how emmeans renames them
        
        table_df <- compute_effect_size_paired(emmeans = emmeans, data = data, 
                                               within_variable = "lureType", response_variable = "rating")
        save_postHoc_t_text(header_row = title, 
                            table_df = table_df, start_col = 2)
        
    # Supplementary Figure 1 RelatednessRating in Pilot ####
  
      svg("SupplFigureRelatednessRating.svg")
  
      plot_data <- pilotFinalSetsDf %>%
        mutate(x = case_when(
          lureType == "perceptually related" & ratingScale == "perceptual relatedness" ~ 1 - dodge,
          lureType == "perceptually related" & ratingScale == "semantic relatedness" ~ 1 + dodge,
          lureType == "semantically related" & ratingScale == "perceptual relatedness" ~ 2 - dodge,
          lureType == "semantically related" & ratingScale == "semantic relatedness" ~ 2 + dodge,
          lureType == "unrelated" & ratingScale == "perceptual relatedness" ~ 3 - dodge,
          lureType == "unrelated" & ratingScale == "semantic relatedness" ~ 3 + dodge,
        ))

      p <- ggplot (data=plot_data, aes(x=lureType, y=rating, fill=ratingScale))+   
        stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
        geom_point(pch = 19, position = position_dodge(0.6), 
                   alpha = 0.2, size=3)+
        geom_line(aes(x = x, group = interaction(Name, lureType)), alpha = 0.1) +
        labs(y="relatedness rating")
      p + theme_classic()+
        #annotation
        #percRel vs semRel in sem rating
        # stars 
        annotate("text", x = 1.75, y = 11, label = "* * *", size =15)+ #!!
        #line
        annotate("path", x = c(1.24, 2.21), y = c(10.8, 10.8), size=1.5) +
        #sem vs new in sem rating
        # stars 
        annotate("text", x = 2.75, y = 11, label = "* * *", size =15)+ #!!
        #line
        annotate("path", x = c(2.24, 3.21),  y = c(10.8, 10.8), size=1.5) +
        #sem vs perc rating in semLure
        # stars 
        annotate("text", x = 2, y = 9.9, label = "* * *", size =15)+ #!!
        #line
        annotate("path", x = c(1.79, 2.21), y = c(9.7, 9.7), size=1.5) +
        # stars 
        annotate("text", x = 1.25, y = 8.9, label = "* * *", size = 15)+ #!!
        #line
        annotate("path", x = c(0.79, 1.76), y = c(8.7, 8.7), size=1.5) +
        #line
        annotate("path", x = c(0.79, 1.21), y = c(7.6, 7.6), size=1.5) +
        # stars 
        annotate("text", x = 1, y = 7.8, label = "* * *", size = 15)+ 
        stat_summary(fun.data = mean_se, geom = "errorbar",  
                     position=position_dodge(0.9),width = 0, size = 1.7)+
        scale_fill_manual(values=c("lightsteelblue", "deepskyblue4"))+ 
        labs(x="lure type")+
        scale_x_discrete(title="rating scale", 
                         labels=c("perceptually \n related","semantically \n related",
                                  "unrelated"))+
        coord_cartesian( ylim = c(0, 11)) + 
        scale_y_continuous(name ="relatedness rating", breaks=c(0, 2, 4, 6, 8, 10))+
        my_theme
      dev.off()
  

    # save SupplementaryFigure1 to source data file ####
      subset_df <- merge(pilotFinalSetsDf, pilotDemoBeforeExclusionDf[, c("Name", "sex")], by = "Name", all.x = TRUE) # include sex in source data file
      
      addWorksheet(wb, "SupplementaryFigure1")
      writeData(wb, "SupplementaryFigure1", subset_df)

      # Save the workbook
      saveWorkbook(wb, "source_data.xlsx", overwrite=TRUE)
      