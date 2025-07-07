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

# ----------------------------------------------------------------------------
# LIBRARY SETUP
# ----------------------------------------------------------------------------

# Import the libraries involved
library(dplyr)                         # For data manipulation
library(stringr)                       # For regex match
library(ggplot2)                       # For drawing graphs
library(corrplot)                      # For drawing correlation matrix
library(lubridate)                     # For date handling
library(zoo)                           # For graph smoothing
library(RColorBrewer)                  # For graph colouring

# Resolve conflicts
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::summarize)

# ----------------------------------------------------------------------------
# Member 3: Lim Beng Rhui (TP068495)
# ----------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATASET RETRIEVAL
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve dataset
member_3_dataset <- quick_preprocessed_dataset()

# Retrieve the involved attribbutes
member_3_dataset <- member_3_dataset %>%
  select(
    Date,
    Year,
    Month,
    Day,
    URL,
    IP,
    Loss
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FURTHER PREPROCESSING BEFORE EDA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# NEW ATTRIBUTE 1: Protocol Type (HTTP and HTTPS)
member_3_dataset <- member_3_dataset %>%
  mutate(
    Protocol_Type = if_else(
      str_detect(URL, "(?i)^https://"),
      "HTTPS",
      if_else(
        str_detect(URL, "(?i)^http://"),
        "HTTP",
        NA_character_
      ))
  )

# Check if there is any NA invoked
sum(is.na(member_3_dataset$Protocol_Type))

# View the empty records
member_3_dataset %>%
  filter(
    is.na(Protocol_Type)
  )

# Only involves empty url, indicating successful retrieval

# NEW ATTRIBUTE 2: Sector (eg: .com for commercial, .org for organization, and etc)
member_3_dataset <- member_3_dataset %>%
  
  # First remove the http and https from the URL
  mutate(
    Sector = case_when(
      str_detect(URL, "^(?i)https?://\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}") ~ NA_character_,
      str_detect(URL, "(?i)https://") ~ str_remove(URL, "(?i)https://"),
      str_detect(URL, "(?i)http://") ~ str_remove(URL, "(?i)http://"),
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Then remove the words after the slash ("/") character, if available
  mutate(
    Sector = sub("^([^/]+).*", "\\1", Sector)
  ) %>%
  
  # Next identify the available sectors
  mutate(
    Sector = case_when(
      str_detect(Sector, "(?i)\\.(com|co|mobi|biz|shop|store|enterprise|ltd|inc)") ~ "Commercial",
      str_detect(Sector, "(?i)\\.(edu|ac|k12|sch|lib)") ~ "Education",
      str_detect(Sector, "(?i)\\.(net|wanadoo)") ~ "Network",
      str_detect(Sector, "(?i)\\.org") ~ "Organization",
      str_detect(Sector, "(?i)\\.(gov|gob|go|govt)") ~ "Government",
      str_detect(Sector, "(?i)\\.mil") ~ "Military",
      str_detect(Sector, "(?i)\\.info") ~ "Information",
      str_detect(Sector, "(?i)\\.name") ~ "Individual",
      str_detect(Sector, "(?i)(tv|media|fm|radio)") ~ "Media",
      str_detect(Sector, "(?i)(museum|art|gallery)") ~ "Cultural",
      str_detect(Sector, "(?i)(blog|wiki|press)") ~ "Publishing",
      str_detect(Sector, "(?i)(tech|dev|app)") ~ "Technology",
      TRUE ~ NA_character_
    )
  )

# Retrieve the number of data for each sector
count(member_3_dataset, Sector)

# NEW ATTRIBUTE 3: File extensions
member_3_dataset <- member_3_dataset %>%
  
  # We first remove the http and https
  mutate(URL_File_Type = case_when(
    str_detect(URL, "(?i)https://") ~ str_remove(URL, "(?i)https://"),
    str_detect(URL, "(?i)http://") ~ str_remove(URL, "(?i)http://"),
    TRUE ~ URL
  )) %>%
  
  # Then we remove the characters in front of the last slash (/) and the text after the question mark (?) - query parameters
  mutate(
    URL_File_Type = case_when(
      str_detect(URL_File_Type, "/") ~ sub(".*/(.*?)(\\?.*)?$", "\\1", URL_File_Type),
      TRUE ~ NA_character_  # For URLs without a path, return empty string
    )
  ) %>%

  # Lastly, we remove the name of the file
  mutate(
    URL_File_Type = case_when(
      str_detect(URL_File_Type, "\\.") ~ str_extract(URL_File_Type, "[^.]+$"),
      TRUE ~ NA_character_
    )
  ) %>%

  # Exist some repetitive names, try to modify them by first converting to lower case
  mutate(
    URL_File_Type = sapply(URL_File_Type, tolower)
  ) %>%
  
  # Then categorize them based on inspection
  mutate(URL_File_Type = case_when(
    
    str_detect(URL_File_Type, "htm|thml|html|php|phtml|asp|jsp|css|js") ~ "Website",
    str_detect(URL_File_Type , "jpg|jpeg|gif|png|mp4") ~ "Media",
    str_detect(URL_File_Type, "txt|pdf|xml|xsl|rdf") ~ "Document",
    str_detect(URL_File_Type, "cgi|pl|cfm") ~ "Script",
    str_detect(URL_File_Type, "sql|db|dat") ~ "Database",
    str_detect(URL_File_Type, "bak|back|old|tmp|sav|log|ds_store|index|bk|prev|bad|ini|inc") ~ "Log",
     
    TRUE ~ NA_character_
    )
  )

# NEW ATTRIBUTE 4: IP Classes
member_3_dataset <- member_3_dataset %>%
  mutate(
    
    # Retrieve the first octet
    first_octet = as.numeric(sub("\\..*", "", IP)),
    
    # Calculate the IP class accordingly
    IP_Class = case_when(
      is.na(IP) ~ NA_character_,
      first_octet < 128 ~ "Class A",
      first_octet < 192 ~ "Class B",
      first_octet < 224 ~ "Class C",
      first_octet < 240 ~ "Class D",
      TRUE ~ "Class E"
    )
  ) %>%
  
  # Remove the column that stores the first octet
  select(-first_octet)

# Done additional cleaning, rearrange the attributes
member_3_dataset <- member_3_dataset %>%
  select(
    Date,
    Year,
    Month,
    Day,
    URL,
    Protocol_Type,
    Sector,
    URL_File_Type,
    IP,
    IP_Class,
    Loss
  )

# Set the data types of the new attributes
member_3_dataset <- member_3_dataset %>%
  mutate(
    Protocol_Type = factor(Protocol_Type),
    Sector = factor(Sector),
    URL_File_Type = factor(URL_File_Type),
    IP_Class = factor(IP_Class)
  )
filter(member_3_dataset, !is.na(URL_File_Type) & !is.na(IP) & !is.na(IP_Class))
summary(member_3_dataset)

# Additional attributes to provide (but abandoned due to long calculation time): 
# - IP Registration Date
# - IP Expiration Date
# - IP Domain Age

# A function to retrieve the registered date of the IP (unused)
extract_registered_date_from_ip <- function(ip) {
  
  # Get the data from whois
  data_from_whois <- system(paste("whois", ip), intern = TRUE)
  
  # Retrieve the associated lines
  registered_dates <- if_else(
    str_detect(data_from_whois, "(?i)(RegDate:|last-modified:|changed:|creation date:|created:)\\s*\\d{4}"),
    trimws(str_extract(data_from_whois, "(?<=:)\\s*.*$")),
    NA_character_
  )
  
  # If no dates found, return NA
  if (length(registered_dates) == 0) {
    return(NA_character_)
    
  } else {
    
    # Remove the NA from the list
    registered_dates <- registered_dates[!is.na(registered_dates)]
  }
  
  # Convert date into correct type
  registered_dates <- parse_date_time(
    registered_dates, 
    orders = c("ymd HMS"),
    truncated = 5
  )
  
  print(min(registered_dates))
  # Return the oldest date
  return(min(registered_dates))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RQ1 - What is the correlation between temporal factors (year, month, and day), website information (protocol type, sector, file type, IP class), and web defacement loss?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Convert the variables into numeric type and ignore records with NA
member_3_correlation_data <- member_3_dataset %>%
  select(Year, Month, Day, Protocol_Type, Sector, URL_File_Type, IP_Class, Loss) %>%
  mutate(
    Year = as.numeric(Year),
    Month = match(Month, month.abb),
    Day = as.numeric(Day),
    Loss = as.numeric(Loss),
    Sector = as.numeric(Sector),
    URL_File_Type = as.numeric(URL_File_Type),
    IP_Class = as.numeric(IP_Class),
    Protocol_Type = as.numeric(Protocol_Type)
  ) %>%
  na.omit()

# Calculate correlation matrix
member_3_cor_matrix <- cor(member_3_correlation_data)

# Visualize correlation matrix
corrplot(member_3_cor_matrix,
         method = "color",           # Fill the entire box
         diag = FALSE,               # Remove diagonal (not useful for analysis)
         addCoef.col = "black",      # Add coefficients to each box except diagonal
         tl.col = "black")           # Change label to black colour instead of red

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RQ2 - How does the average web defacement losses vary for different protocol types across different years?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate the aggregated yearly loss for each protocol type
member_3_average_yearly_loss_based_on_protocol <- member_3_dataset %>%
  filter(!is.na(Protocol_Type)) %>%                                              # Remove NA values
  group_by(Year, Protocol_Type) %>%                                              # Group data by year and protocol type
  summarise(mean_loss = mean(Loss, na.rm = TRUE), .groups = "drop") %>%          # Calculate mean and remove the grouping
  group_by(Protocol_Type) %>%                                                    # Regroup data for smoothing purpose
  arrange(Year) %>%                                                              # Arrange the data by year
  mutate(                                                                        # Perform smoothing to better display trend
    smoothed_loss = rollmean(mean_loss, k = 3, fill = NA, align = "center")
  ) %>%
  ungroup()                                                                      # Ungroup to avoid graph not working
 
# Generate plot with the data
ggplot(member_3_average_yearly_loss_based_on_protocol, aes(x = Year)) +

  geom_line(aes(y = mean_loss, color = Protocol_Type), linetype = "dashed") +           # Dashed line (before smoothing)
  geom_line(aes(y = smoothed_loss, color = Protocol_Type), size = 1.2, na.rm = TRUE) +  # Solid line (smoothed)
  geom_point(aes(y = smoothed_loss, color = Protocol_Type), size = 3, na.rm = TRUE) +   # Data points (smoothed)
  
  labs(
    title = "Average Loss based on Protocol Type",    # Title
    x = "Year",                                       # x-axis label
    y = "Average Loss",                               # y-axis label
    color = "Protocol Type"                           # Legend
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),                              # Center title
    axis.title.x = element_text(margin = margin(t = 20), face = "bold"),                # Add space for x-axis
    axis.title.y = element_text(margin = margin(r = 20), face = "bold"),                # Add space for y-axis
    legend.box.background = element_rect(color = "black", size = 0.5),                  # Box the legend
    legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5)                              # Add margin to legend box
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RQ3 - What is the relative contribution of each URL sector to the total web defacement loss across 5-year periods?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate total loss by 5-year periods and sector
member_3_aggregated_5_year_loss_by_sector <- member_3_dataset %>%
  filter(!is.na(Sector)) %>%                                                   # Remove unlabelled sectors
  mutate(
    Period = paste0(floor(Year/5)*5, "-", floor(Year/5)*5 + 4)                 # Separate period into 5 years
  ) %>%
  group_by(Period, Sector) %>%                                                 # Group data by the 5 year period with sector
  summarise(Total_Loss = sum(Loss, na.rm = TRUE), .groups = "drop") %>%        # Calculate total loss for each combination
  arrange(Period)                                                              # Arrange the period ascendingly

# Create visualization using stacked bar chart
ggplot(member_3_aggregated_5_year_loss_by_sector, aes(x = Period, y = Total_Loss, fill = Sector)) +
  geom_bar(position = "fill", stat = "identity", width = 0.7) +                # Create stacked bar plots 
  scale_y_continuous(labels = scales::percent) +                               # Convert proportion into percentage
  scale_fill_brewer(palette = "Set3") +                                        # Set colour
  labs(
    title = "Distribution of Losses by Sector per 5-Year Period",              # Title
    x = "Time Period",                                                         # x-axis label
    y = "Percentage of Total Loss",                                            # y-axis label
    fill = "Sector"                                                            # Legend
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),                     # Center title
    axis.title.x = element_text(margin = margin(t = 20), face = "bold"),       # Add space for x-axis
    axis.title.y = element_text(margin = margin(r = 20), face = "bold"),       # Add space for y-axis
    legend.box.background = element_rect(color = "black", size = 0.5),         # Box the legend
    legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5)                     # Add margin to legend box
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RQ4 - What monthly trends exist in the loss distribution among different IP classes? 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate median for midpoint before plotting
median_loss <- member_3_dataset %>%
  filter(!is.na(IP_Class), IP_Class != "Class E") %>%
  group_by(Month, IP_Class) %>%
  summarize(total_loss = sum(Loss)) %>%
  pull(total_loss) %>%
  median()

# Create heatmap
member_3_dataset %>%
  filter(!is.na(IP_Class), IP_Class != "Class E") %>%
  group_by(Month, IP_Class) %>%
  summarize(total_loss = sum(Loss), .groups = "drop") %>%
  mutate(
    Month = factor(month.abb[Month], 
                   levels = month.abb),
    loss_formatted = scales::comma(total_loss)
  ) %>%
  ggplot(aes(x = Month, y = IP_Class, fill = total_loss)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "white",
    mid = "lightyellow",
    high = "red",
    midpoint = median_loss,
    labels = scales::comma
  ) +
  labs(
    title = "Monthly Loss Patterns by IP Class",
    subtitle = "Total Loss Amount by Month and IP Class",
    x = "Month",
    y = "IP Class",
    fill = "Total Loss ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.text.x = element_text(angle = 0),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RQ5 - File Type vs Mean Loss over Across Years (not in report - additional)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create a custom smoothing function
custom_smooth <- function(df, smooth_specs) {
  df %>%
    group_by(URL_File_Type) %>%
    arrange(Year) %>%
    mutate(smoothed_loss = case_when(
      URL_File_Type == "Database" ~ rollmean(mean_loss, k = smooth_specs["Database"], fill = NA, align = "center"),
      URL_File_Type == "Document" ~ rollmean(mean_loss, k = smooth_specs["Document"], fill = NA, align = "center"),
      URL_File_Type == "Log" ~ rollmean(mean_loss, k = smooth_specs["Log"], fill = NA, align = "center"),
      URL_File_Type == "Media" ~ rollmean(mean_loss, k = smooth_specs["Media"], fill = NA, align = "center"),
      URL_File_Type == "Script" ~ rollmean(mean_loss, k = smooth_specs["Script"], fill = NA, align = "center"),
      URL_File_Type == "Website" ~ rollmean(mean_loss, k = smooth_specs["Website"], fill = NA, align = "center"),
      TRUE ~ mean_loss
    )) %>%
    ungroup()
}

# Prepare the data
smoothed_mean_loss <- member_3_dataset %>%
  filter(!is.na(URL_File_Type)) %>%
  group_by(URL_File_Type, Year) %>%
  summarise(mean_loss = mean(Loss), .groups = 'drop') %>%
  arrange(URL_File_Type, Year)

# Specify custom smoothing windows
smooth_specs <- c(
  "Database" = 2,   # 2-year window
  "Document" = 5,   # 4-year window
  "Log" = 3,        # 2-year window
  "Media" = 3,      # 3-year window
  "Script" = 3,     # 3-year window
  "Website" = 5     # 4-year window
)

# Apply custom smoothing
smoothed_mean_loss <- custom_smooth(smoothed_mean_loss, smooth_specs)

# Visualization
ggplot(smoothed_mean_loss, aes(x = Year)) +
  geom_line(aes(y = mean_loss, color = "Original"), size = 1, alpha = 0.5, na.rm = TRUE) +
  geom_point(aes(y = mean_loss, color = "Original"), size = 2, alpha = 0.5, na.rm = TRUE) +
  geom_line(aes(y = smoothed_loss, color = "Smoothed"), size = 1.5, na.rm = TRUE) +
  geom_point(aes(y = smoothed_loss, color = "Smoothed"), size = 3, na.rm = TRUE) +
  facet_wrap(~ URL_File_Type, 
             ncol = ceiling(sqrt(length(unique(smoothed_mean_loss$URL_File_Type)))),
             scales = "free_y"
  ) +
  labs(
    title = "Yearly Mean Loss with Custom Smoothing",
    x = "Year",
    y = "Mean Loss",
    color = "Data Type"
  ) +
  scale_color_manual(values = c("Original" = "gray", "Smoothed" = "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
