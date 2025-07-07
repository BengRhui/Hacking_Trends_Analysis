# 1. Chan Chun Ming  (TP068983)
# 2. Eu Jun Hong     (TP068580)
# 3. Lim Beng Rhui   (TP068495)
# 4. Ng Xuan Jack    (TP067678)

# Set working directory (modify if needed)
setwd("~/Documents/RStudio/PFDA Assignment")

# Load packages into session
library(MASS)         # For transformation
library(tidyverse)    # For data manipulation
library(dplyr)        # For data cleaning
library(VIM)          # Using kNN to predict missing value
library(countrycode)  # Standardize country name
library(stringdist)   # Fuzzy matching to match mispelled or similar value
library(lubridate)    # Helps to handle date-related issues
library(moments)      # To calculate skewness
library(mice)         # To impute empty data

# Resolve conflicts for functions (due to MASS library)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# Load dataset to RStudio
hacking_data <- read.csv("hackingData.csv")

# ----------------------------------------------------------------------------
# ADDITIONAL FUNCTIONS AND VARIABLES FOR DATA CLEANING PURPOSES
# ----------------------------------------------------------------------------

# Retrieve list of country names
country_name_list <- unique(
  na.omit(countrycode::codelist$country.name.en)
)

# Function that replace all invalid values with NA
replace_missing <- function(dataSet, columns) {
  dataSet %>%
    mutate(across(
      all_of(columns), 
      ~ if_else(
        str_detect(as.character(.), "(?i)^Unkno|^null") | as.character(.) == "" | is.na(.),   # Detect empty values
        NA_character_,                                                                        # Replace empty values with NA
        .                                                                                     # Remain the initial value if not empty
    )))                                                                                    
}

# Function to standardize country name
standardize_country <- function(country) {
  
  # Return NA if input is missing
  if (is.na(country)) return(NA) 
  
  # Use Jaro-Winkler distance to calculate match index
  distances <- stringdist::stringdist(
    tolower(country),                     # String to be compared with
    tolower(country_name_list),           # The list to be compared from
    method = "jw"                         # Method used is Jaro-Winkler string distance
  )
  
  # Retrieve the country name with smallest distance (highest similarity)
  best_index <- which.min(distances)
  best_match <- country_name_list[best_index]
  
  # Suggestion: Might include a threshold to check if the country is invalid
  # Example of code:
  # best_distance <- distances[best_index]
  # if (best_distance > threshold) return(NA)
  
  # Return the most matching country name
  return(best_match)
}

# Function to check if IP address is in a valid form
get_formatted_ip <- function(ip_address) {
  
  # For NA, return the same value
  if (is.na(ip_address) || ip_address == "")  return (NA)
  
  # First check if the IP is in the correct format
  format <- "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$"                          # ^ for beginning, $ for ending, \\d for one or more digits, \\. for dot
  if (!str_detect(ip_address, format)) {
    return (NA)
  }
  
  # Split the input into octets based on the dots
  splitted_ip <- str_split(ip_address, "\\.", simplify = TRUE)     # Split address based on dot, then use simplify to convert to matrix (easier to process later)
  splitted_ip <- as.numeric(splitted_ip)
  
  # Check if each octet is between 0 to 255
  if (all(splitted_ip >= 0 & splitted_ip <= 255)) return (ip_address)
  else return (NA)
}

# A function to retrieve their version of operating system
retrieve_os_version <- function(os_string, family) {
  
  # Remove white spaces of the string
  os_string <- trimws(os_string)
  
  # Retrieve the split list
  os_split <- str_split(os_string, " ")[[1]]
  
  # Define regex for each os family
  regex <- switch(family,
                  "Windows" = "windows|win",
                  "Linux" = "linux",
                  "macOS" = "macos|macosx|osx",
                  "BSD" = "bsd|freeb|openbsd|netbs",
                  "Solaris" = "solar|sunos",
                  "Unix" = "aix|hp-ux|tru64|irix|sco unix|unix",
                  "Cisco" = "ios|cisco|juniper|junos",
                  "Android" = "android",
                  "NetWare" = "netware",
                  "OS/2" = "os2",
                  "Embedded" = "embedded|wap|router|firewall|f5 b|ipxe|citri|crestron|hp|modem|adapter|device|sgos",
                  NA)
  
  # Return NA if family is NA
  if (is.na(regex)) {
    return(NA_character_)
  }
  
  # Loop through each element
  for (index in 1:length(os_split)) {
    
    # If the element matches the rergex
    if(!is.na(str_match(os_split[index], regex))) {
      
      # Further check if the element is not the last one
      if (index < length(os_split)) {
        
        # Retrieve the version
        version <- os_split[index + 1]
        
        # Perform modification for different os family - modify for Windows family
        if (family == "Windows") {
          if (!is.na(str_match(os_string, "7"))) version = "7"
          else if (!is.na(str_match(os_string, "2003"))) version = "2003"
          else if (!is.na(str_match(os_string, "pocketpc/ce"))) version = NA_character_
          
        # Modify for BSD family
        } else if (family == "BSD") {
          match_result <- str_match(os_string, "(\\d\\.\\d\\.\\d)") # Capture group added
          if (!is.na(match_result[1, 1])) { # Check the first element of the matrix
            version <- match_result[1, 1]  # Extract the matched value from the matrix
          } else {
            version <- NA_character_ # Or some other default value
          }
          
        # Modify for other faimly
        } else if (family == "Embedded" || family == "Cisco" || family == "Unix") {
          match_result <- str_match(os_string, "\\d+\\..")
          if (!is.na(match_result[1, 1])) version <- match_result[1, 1]
          else version <- NA_character_
        
        # Modify for solaris family
        } else if (family == "Solaris") {
          match_result <- str_match(version, "(\\d+)/(\\d+)")
          if (!is.na(match_result[1, 3]) && match_result[1, 3] != "") {
            version_last_part <- match_result[1, 3]
          }
        }
        
        # Return the version after modification
        return(version)
      }
    }
  }
  
  # Return NA if condition is not fulfilled
  return(NA_character_)
}

# ----------------------------------------------------------------------------
# DATASET OVERVIEW BEFORE CLEANING
# ----------------------------------------------------------------------------

# Inspecting dataset before cleaning
view(hacking_data)                             # Open a window to view the dataset in table format
summary(hacking_data)                          # Gives an overview of the data
head(hacking_data, 20)                         # Get glimpse of the first 20 rows of dataset

data.frame(                                    # Inspect data type and the number of unique values for each column
  row.names = NULL,
  Attributes = colnames(hacking_data),
  Data_Type = sapply(hacking_data, class), 
  Unique_Value = sapply(hacking_data, dplyr::n_distinct)
)

options(max.print = 1000)
lapply(                                   # View the unique values inside each column, where colnames(hacking_data) can be replaced with attribute names (e.g. "Date")
  hacking_data[colnames(hacking_data)],
  function(x) sort(unique(x))
)

# Small note: 
# Empty values exist as "", "Anonymous Proxy" (for Country), "Unkno", "unknown", "Unknown"

# ----------------------------------------------------------------------------
# DATASET CLEANING
# ----------------------------------------------------------------------------

# Step 1: Only keep the attributes that we investigate
hacking_data_preprocessed <- hacking_data %>%
  select(
    "Date",
    "URL",
    "IP",
    "Country",
    "OS",
    "WebServer",
    "DownTime",
    "Loss"
  )

# Step 2: Remove duplicates
nrow(hacking_data_preprocessed) - n_distinct(hacking_data_preprocessed)       # Number of duplicated rows: 1192 rows
hacking_data_preprocessed <- distinct(hacking_data_preprocessed)              # Only consider distinct rows

# Step 3: Replace empty values (except DownTime and Loss) with NA
hacking_data_preprocessed <- replace_missing(
  hacking_data_preprocessed, 
  c("Date", "URL", "IP", "Country", "OS", "WebServer")
)

# Step 4: Check if there is any rows with all NA
any(rowSums(is.na(hacking_data)) == ncol(hacking_data))             # Calculate the number of NA in a row and compare with number of columns
                                                                    # No empty rows exist in this case
# Step 5: Deal with issues in each column
# For Date, the issues are:
# - Date is not consistent, contains date in format of 2/1/1998 and 14/01/1998
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(across(
    all_of("Date"),              # Get each value in the date column
    dmy                          # dmy is from the lubridate package, helps to standardize date in date-month-year format
  ))

# Add additional attributes for year, month and date
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE),
    Day = day(Date)
  )

# For URL, the issues are:
# - Empty spaces at the front of data
# - Contains uncleaned URL (e.g. "Freestyler??http://hebust.edu.cn", "fs - Freestyler??http://www.unse.edu.ar")
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    URL = sub(".*?(http)", "\\1", URL)         # Check if there's any characters before the first occurrence ("\\1") of "http", remove if yes
  )

# For IP, no issues is found at first glance.
# - Try check if there is any invalid IP (if any octet does not fall between 0 to 255)
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    IP = sapply(IP, get_formatted_ip)        # Use function to check for IP address validity, if not valid then replace with NA
  )

# For Country, the issues are:
# - Name written in different formats, e.g. "Taiwan", "TAIWAN", and "Taiwan, Prov"
# - Some does not represent country, e.g. "AMERICANSAMOA", "ASIA/PACIFIC REGION")
# - There are anonymous data, i.e. "Anonymous Proxy"

# First try to observe if the countries are transformed properly
country_test <- hacking_data_preprocessed %>%
  select("Country") %>%
  mutate(
    New_Country = sapply(Country, standardize_country)
  )
distinct(country_test)

# Notice that some countries are transformed incorrectly, and manual intervention is required

# Capitalize the countries
hacking_data_preprocessed$Country <- str_to_title(hacking_data_preprocessed$Country)

# We first create a new column to store continents (to avoid data loss)
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    Continent = case_when(
      Country %in% c("Europe", "European Uni", "European Union", "Westeuro", "Easteuro") ~ "Europe",
      Country %in% c("Asia", "Asia/Pacific Region", "Middleeast") ~ "Asia",
      Country == "Oseania" ~ "Oceania",
      Country == "Southamerica" ~ "Americas",
      Country == "Africa" ~ "Africa",
      TRUE ~ NA_character_
  ))

# Next, modify the country for those that cannot be detected
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(Country = case_when(
    Country == "Yugoslavia" ~ "Serbia",
    Country == "Virginislands(British)" ~ "British Virgin Islands",
    Country == "Macedonia" | Country == "Macedonia, T" ~ "North Macedonia",
    Country == "Macau" ~ "Macao SAR China",
    Country == "Ascensionisland" ~ "United Kingdom",
    Country == "Congo" ~ "Congo - Kinshasa",
    Country == "Virginislands(U.s.)" ~ "U.S. Virgin Islands",
    Country == "Micronesia" ~ "Micronesia (Federated States of)",
    Country == "Easttimor" ~ "Timor-Leste",
    Country == "Saotomeandprincipe" ~ "São Tomé & Príncipe",
    Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Korea, Repub" | Country == "Korea" ~ "South Korea",
    Country == "America" ~ "United States",
    Country %in% c("Europe", "Asia/Pacific Region", "Anonymous Proxy", 
                   "Satellite Provider", "European Uni", "European Union",
                   "Virgin Islands", "Westeuro", "Easteuro", "Southamerica",
                   "Asia", "Oseania", "Middleeast", "Africa") ~ NA_character_,
    TRUE ~ Country
  ))

# Now use the standardize_country function to transform country
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    Country = sapply(Country, standardize_country)
  )

# Then, we map countries to continents using countrycode but skip NA
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    Continent = case_when(
      (!is.na(Country) & is.na(Continent)) ~ countrycode(Country, "country.name", "continent"),
      TRUE ~ Continent  # Keep existing values
    )
  )

# For OS, the issues are:
# - Name written in different ways, e.g. "windows", "Windows", "WINDOWS"
# - Some might not be valid OS, e.g. "OVH/Shared", "Vonage V-Portal VoIP adapter", "Crestron XPanel control system"

# First convert the data to small case and remove any empty spaces if there is
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(OS = str_to_lower(OS)) %>%
  mutate(OS = lapply(OS, trimws))

# Then retrieve the OS family from OS
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    OS_Family = case_when(
      str_detect(OS, "windows|win") ~ "Windows",
      str_detect(OS, "linux") ~ "Linux",
      str_detect(OS, "macos|macosx|osx") ~ "macOS",
      str_detect(OS, "bsd|freeb|openbsd|netbs") ~ "BSD",
      str_detect(OS, "solar|sunos") ~ "Solaris",
      str_detect(OS, "aix|hp-ux|tru64|irix|sco unix|unix") ~ "Unix",
      str_detect(OS, "ios|cisco|juniper|junos") ~ "Cisco",
      str_detect(OS, "netware") ~ "NetWare",
      str_detect(OS, "os2") ~ "OS/2",
      str_detect(OS, "embedded|wap|router|firewall|f5|ipxe|citri|crestron|hp|modem|adapter|device|sgos") ~ "Embedded",
      TRUE ~ NA_character_,
  ))

# Also retrieve the version from OS and remove the initial column
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(OS_Version = mapply(retrieve_os_version, OS, OS_Family)) %>%
  select(-OS)

# Observe the number of records for each OS
count(hacking_data_preprocessed, OS_Family)

# Convert OS with small quantity as others
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    OS_Family = if_else(str_detect(OS_Family, "(?i)Cisco|Embedded|macOS|NetWare|OS/2"),
                        "Others",
                        OS_Family)
  )

# For WebServer, the issues are:
# - Contains invalid data, e.g. "*****************", "+", "WebServer", "WebSite"
# - Might contain other information, e.g. IP address ("216.74.154.150"), URL ("Advanced Hosting by http://www.unixy.net/varnish" )

hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(WebServer_New = WebServer)

# Place url to the URL column if the URL column is empty
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    URL = if_else(
      !is.na(WebServer) & is.na(URL) & !is.na(str_extract(WebServer, "(?i)http[s]?://\\S+|www.\\S+|\\S+\\.com|\\S+\\.net")), 
      str_extract(WebServer, "(?i)http[s]?://\\S+|www.\\S+|\\S+\\.com|\\S+\\.net"), 
      URL),  
    WebServer = if_else(
      !is.na(WebServer) & str_detect(WebServer, "(?i)http[s]?://\\S+|www.\\S+|\\S+\\.com|\\S+\\.net"), 
      NA_character_, 
      WebServer)
  )

# Place IP address to the correct column
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    IP = if_else(
      is.na(IP) & !is.na(WebServer) & str_detect(WebServer, "(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})"),
      WebServer,
      IP
    ),
    WebServer = if_else(
      !is.na(WebServer) & str_detect(WebServer, "(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})"),
      NA_character_,
      WebServer
    )
  )

# Remove invalid data
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    WebServer = if_else(
      !str_detect(WebServer, "[a-zA-Z0-9]") | !is.na(str_match(WebServer, "&quot")),
      NA_character_,
      WebServer
    )
  )

# Extract version in format of x.x.x to new list
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    WebServer_Version = str_extract(WebServer, "(\\d+(\\.\\d+)+)"),
    WebServer = str_replace_all(WebServer, "v?(\\d+(\\.\\d+)+)(\\s*\\S+)*", "")
  )

# Remove all ending "/" and "-"
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    WebServer = sapply(str_replace_all(WebServer, "[/-]+$", ""), trimws)
  ) 

# Standardize the common webservers
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    WebServer = case_when(
      str_detect(WebServer, "(?i)^apache") ~ "Apache",
      str_detect(WebServer, "(?i)microsoft.*iis|^iis|microsoftoffice|pws") ~ "Microsoft-IIS",
      str_detect(WebServer, "(?i)nginx|ngx_openresty|chamber88") ~ "nginx",
      str_detect(WebServer, "(?i)sun.*web|sun.*one|oracle.*web|oracle.*iplanet|glassfish|solarissunos|oracle") ~ "Sun-Oracle-Web-Server",
      str_detect(WebServer, "(?i)^litespeed$") ~ "LiteSpeed",
      str_detect(WebServer, "(?i)^zeus$") ~ "Zeus",
      str_detect(WebServer, "(?i)^lighttpd$") ~ "lighttpd",
      str_detect(WebServer, "(?i)^akamaighost$") ~ "AkamaiGHost",
      str_detect(WebServer, "(?i)^varnish$") ~ "Varnish",
      str_detect(WebServer, "(?i)gws|ghs") ~ "Google-Web-Server",
      str_detect(WebServer, "(?i)cloudflare") ~ "Cloudflare",
      str_detect(WebServer, "(?i)lotus|domino") ~ "Lotus-Domino",
      str_detect(WebServer, "(?i)thttpd") ~ "thttpd",
      is.na(WebServer) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

# Step 6: Rearrange the attributes (place IV at the back)
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  select(
    Date,
    Year,
    Month,
    Day,
    URL,
    IP,
    Continent,
    Country,
    OS_Family,
    OS_Version,
    WebServer,
    WebServer_Version,
    DownTime,
    Loss
  )

# ----------------------------------------------------------------------------
# DISTRIBUTION AND OUTLIER HANDLING - DOWNTIME
# ----------------------------------------------------------------------------

# Step 1: Have a glance at the downtime attribute using simple plots: boxplot and histogram
boxplot(hacking_data_preprocessed$DownTime)
hist(hacking_data_preprocessed$DownTime)

# Step 1a: Data seems normally distributed, calculate the skewness of the data
skewness(hacking_data_preprocessed$DownTime)           # Skewness: 0.1384, which is close to normal distribution. Don't need to treat for skewness

# Step 2: Use IQR to check for outliers
downtime_IQR <- IQR(hacking_data_preprocessed$DownTime, na.rm = TRUE)
cat("Lower bound smaller than minimum:",
    (quantile(hacking_data_preprocessed$DownTime, 0.25) - (1.5 * downtime_IQR)) <= min(hacking_data_preprocessed$DownTime) ,
    "\n",
    "Upper bound larger than maximum:",
    (quantile(hacking_data_preprocessed$DownTime, 0.75) + (1.5 * downtime_IQR)) >= max(hacking_data_preprocessed$DownTime),
    "\n"
  )

# Since the boundaries are -14.5 and 77.5, which exceeds the range of the data, no outliers are detected using IQR. 

# Step 3: Check for outliers using z-score
downtime_z_scaled <- scale(hacking_data_preprocessed$DownTime)
cat("Exist z-score lower than -3:",
    min(downtime_z_scaled) < -3,
    "\n",
    "Exist z-score greater than 3:",
    max(downtime_z_scaled) > 3,
    "\n"
)

# ----------------------------------------------------------------------------
# HANDLING EMPTY VALUES - LOSS
# ----------------------------------------------------------------------------

# Step 1: Investigate the number of empty values in loss
cat(
  "No of empty data:", 
  sum(is.na(hacking_data_preprocessed$Loss)),
  "(",
  sum(is.na(hacking_data_preprocessed$Loss)) / length(hacking_data_preprocessed$Loss) * 100,
  "% )\n"
)

# Since roughly 15.85% (33591 instances) of loss is missing, we can use MICE to impute data

# Create predictor matrix
pred_matrix <- make.predictorMatrix(hacking_data_preprocessed)

# Perform imputation
mice_imputation <- mice(hacking_data_preprocessed,
                        m = 3,                        # Create 3 datasets with imputed values
                        maxit = 20,                   # Perform up to 20 times of iteration
                        method = "pmm",               # Uses predictive mean matching as the imputation method
                        pred = pred_matrix)           # The variables involved

# Save the imputed data to the dataset
hacking_data_preprocessed <- complete(mice_imputation)

# Check if the imputation is successful
cat(
  "Imputation successful:",
  sum(is.na(hacking_data_preprocessed$Loss)) == 0,
  "\n"
)

# ----------------------------------------------------------------------------
# DISTRIBUTION AND OUTLIER HANDLING - LOSS
# ----------------------------------------------------------------------------

# Step 0: Convert loss to numeric to ensure it can be used to count IQR
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(
    Loss = as.numeric(Loss)
  )

# Step 1: Have a glance at the downtime attribute using simple plots: boxplot and histogram
boxplot(hacking_data_preprocessed$Loss)
hist(hacking_data_preprocessed$Loss)

# Step 1a: Data is definitely not normally distributed, calculate the skewness of the data
skewness(hacking_data_preprocessed$Loss, na.rm = TRUE)           # Skewness: 2.3309, which is extremely right-skewed. Need to perform transformation to address skewness

# Step 2: Perform different transformation to determine the best
# Step 2a: Perform log transformation
transformation_test <- hacking_data_preprocessed
transformation_test$Loss <- log1p(transformation_test$Loss)
transformation_log_skewness <- skewness(transformation_test$Loss, na.rm = TRUE)

# Step 2b: Perform square root transformation
transformation_test <- hacking_data_preprocessed
transformation_test$Loss <- sqrt(transformation_test$Loss)
transformation_sqrt_skewness <- skewness(transformation_test$Loss, na.rm = TRUE)

# Step 2c: Perform cube root transformation
transformation_test <- hacking_data_preprocessed
transformation_test$Loss <- (transformation_test$Loss) ^ (1/3)
transformation_cbrt_skewness <- skewness(transformation_test$Loss, na.rm = TRUE)

# Step 2d: Perform box-cox transformation
transformation_test <- hacking_data_preprocessed
boxcox_result <- boxcox(Loss ~ 1, data = transformation_test)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]               # lambda = 2/9 in this case
transformation_test$Loss <- (transformation_test$Loss^lambda - 1) / lambda
transformation_box_cox_skewness <- skewness(transformation_test$Loss, na.rm = TRUE)

# Compare results
data.frame(
  Method = c("Log", "Square root", "Cube root", "Box-cox"),
  Skewness = c(
    transformation_log_skewness, 
    transformation_sqrt_skewness, 
    transformation_cbrt_skewness, 
    transformation_box_cox_skewness)
)

# Since box-cox yields the best result (closest to 0), applying box-cox to the dataset
boxcox_result <- boxcox(Loss ~ 1, data = hacking_data_preprocessed)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
hacking_data_preprocessed$Loss <- (hacking_data_preprocessed$Loss^lambda - 1) / lambda

# Step 2a: Use IQR to check for outliers
loss_IQR <- IQR(hacking_data_preprocessed$Loss, na.rm = TRUE)
loss_IQR_lower <- quantile(hacking_data_preprocessed$Loss, 0.25, na.rm = TRUE) - (1.5 * loss_IQR)
loss_IQR_upper <- quantile(hacking_data_preprocessed$Loss, 0.75, na.rm = TRUE) + (1.5 * loss_IQR)

# Check if there exist any extreme values
cat("Lower bound smaller than minimum:",
    loss_IQR_lower <= min(hacking_data_preprocessed$Loss, na.rm = TRUE) ,
    "\n",
    "Upper bound larger than maximum:",
    loss_IQR_upper >= max(hacking_data_preprocessed$Loss, na.rm = TRUE),
    "\n"
)

# Step 2a - sub: Inspect the number of values that does not fall between the boundaries, since there exist such values
cat("Percentage of data exceeding IQR boundaries:",
    sum(
      hacking_data_preprocessed$Loss > loss_IQR_upper | 
        hacking_data_preprocessed$Loss < loss_IQR_lower, 
        na.rm = TRUE) / 
    sum(hacking_data_preprocessed$Loss, na.rm = TRUE) * 100,
    "%\n")

# 3069 records, roughly 0.11%. Can be removed, but better to use winsorization technique to modify data
hacking_data_preprocessed <- hacking_data_preprocessed %>%
  mutate(New_Loss = case_when(
    Loss > loss_IQR_upper ~ loss_IQR_upper,
    Loss < loss_IQR_lower ~ loss_IQR_lower,
    TRUE ~ Loss
  ))

# ----------------------------------------------------------------------------
# EXPORTNG CLEANED DATASET
# ----------------------------------------------------------------------------

# To save time from preprocessing the dataset, we'll be exporting the cleaned dataset and use it for the upcoming EDA
write.csv(hacking_data_preprocessed, "cleaned_hacking_data.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# QUICK PREPROCESSING CLEANED DATASET
# ----------------------------------------------------------------------------

# Define a function to retrieve the cleaned dataset -> everyone will be using this dataset for EDA
quick_preprocessed_dataset <- function() {
  
  # Set working directory (again)
  setwd("~/Documents/RStudio/PFDA Assignment")
  
  # Read the cleaned dataset
  dataset_cleaned <- read.csv("cleaned_hacking_data.csv")
  
  # Change data type
  dataset_cleaned <- dataset_cleaned %>%
    mutate(
      Date = as.Date(Date),
      Year = factor(Year),
      Month = factor(Month),
      Day = factor(Day),
      Country = factor(Country),
      WebServer = factor(WebServer),
      Continent = factor(Continent),
      OS_Family = factor(OS_Family)
    )
  
  # Return the dataset
  return (dataset_cleaned)
}