# Clear environment
rm(list = ls())

# Set seed for reproducibility
set.seed(103)

# Load required libraries
libs <- c(
  'data.table', 'readr', 'dplyr', 'rpart', 'gtools', 'lubridate', 'randomForest',
  'caret', 'rpart.plot', 'rattle', 'RColorBrewer'
)

lapply(libs, require, character.only = TRUE)

# Read command line arguments
myArgs <- commandArgs(trailingOnly = TRUE)
feature_file_location <- myArgs[1]
iteration_identifier <- myArgs[2]
output_folder_location <- myArgs[3]
output_file_location <- myArgs[4]
var_theme_mapping_location <- myArgs[5]
prod_level <- myArgs[6]
feature_file_location_all <- myArgs[7]

# Constants
CORR_TH <- 0.85
SD_OUTLIER <- 2
PLOT <- FALSE
CUST_CUT_OFF <- 4

"Xnix" <- Negate("%in%")

# Read input files
df_cust_fp <- read.csv(file = feature_file_location, header = TRUE, stringsAsFactors = FALSE)
df_cust_fp_all <- read.csv(file = feature_file_location_all, header = TRUE, stringsAsFactors = FALSE)

print("Num of input rows:")
print(nrow(df_cust_fp))

prod_reg_list <- unique(df_cust_fp$cluster_level)
print("prod_reg_list:")
print(prod_reg_list)

var_theme <- read.csv(file = var_theme_mapping_location, header = TRUE, stringsAsFactors = FALSE)

# Data preparation function
data_prep <- function(df) {
  df[factor_cols] <- lapply(df[factor_cols], factor)
  if (length(num_cols) != 0) {
    df[num_cols] <- lapply(df[num_cols], as.numeric)
  }
  df[int_cols] <- lapply(df[int_cols], as.integer)
  return(df)
}

# Functions to get optimal complexity parameter (cp) and minimum error
get_cp <- function(x) {
  min_index <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min_index, "CP"]
  return(cp)
}

get_min_error <- function(x) {
  min_index <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min_index, "xerror"]
  return(xerror)
}

# Define variable types
factor_cols <- var_theme$variable[var_theme$type == 'factor_cols']
num_cols <- var_theme$variable[(var_theme$type == 'num_cols') & !(var_theme$theme %in% c('id', 'target'))]
int_cols <- var_theme$variable[(var_theme$type == 'int_cols') & !(var_theme$theme %in% c('id', 'target'))]
id_cols <- var_theme$variable[var_theme$theme == "id"]
target <- var_theme$variable[var_theme$theme == 'target']

df_cust_fp_2 <- df_cust_fp[, var_theme$variable]
df_cust_fp_all_2 <- df_cust_fp_all[, var_theme$variable]

print("Starting Data Prep")
df_cust_fp_2 <- data_prep(df_cust_fp_2)
df_cust_fp_all_2 <- data_prep(df_cust_fp_all_2)

print(nrow(df_cust_fp_2))
print(colnames(df_cust_fp_2))

# Main function to process product data
smc_prod <- function(product) {
  print(paste("Now Running", product))
  df_prod <- df_cust_fp_2[df_cust_fp_2$cluster_level == product, ]
  df_prod_all <- df_cust_fp_all_2[df_cust_fp_all_2$cluster_level == product, ]
  
  if (sum(df_prod$markup, na.rm = TRUE) == 0) {
    print("Exiting")
    return(NULL)
  }
  
  print(nrow(df_prod))
  
  var_theme_temp <- var_theme[var_theme[product] == 1, ]
  factor_cols_temp <- var_theme_temp$variable[var_theme_temp$type == 'factor_cols']
  num_cols_temp <- var_theme_temp$variable[(var_theme_temp$type == 'num_cols') & !(var_theme_temp$theme %in% c('id', 'target'))]
  int_cols_temp <- var_theme_temp$variable[(var_theme_temp$type == 'int_cols') & !(var_theme_temp$theme %in% c('id', 'target'))]
  id_cols_temp <- var_theme_temp$variable[var_theme_temp$theme == 'id']
  target_temp <- var_theme_temp$variable[var_theme_temp$theme == 'target']
  
  df_prod_prpd <- df_prod[, c(var_theme_temp$variable)]
  x_cols <- setdiff(names(df_prod_prpd), target_temp)
  fmla <- as.formula(paste(target_temp, paste(x_cols, collapse = "+"), sep = "~"))
  
  # Decision Tree Modeling
  cart_model <- rpart(
    formula = fmla,
    data = df_prod,
    method = "anova",
    control = list(minbucket = 10, maxdepth = 5, cp = 0.01)
  )
  
  # Extract feature importance
  importance_df <- data.frame(cart_model$variable.importance)
  importance_df$vars <- row.names(importance_df)
  row.names(importance_df) <- NULL
  
  # Save output files
  output_file <- file.path(output_folder_location, paste("results_", product, ".csv", sep=""))
  write.csv(df_prod, file = output_file, row.names = FALSE)
  
  return(df_prod)
}

# Run for each product
run_res <- do.call("rbind", lapply(prod_reg_list, smc_prod))

# Save final results
write.csv(run_res, file = output_file_location, row.names = FALSE)
