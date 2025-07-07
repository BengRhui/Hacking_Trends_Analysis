# ----------------------------------------------------------------------------
# QUICK PREPROCESSING CLEANED DATASET
# ----------------------------------------------------------------------------

# Define a function to retrieve the cleaned dataset -> everyone will be using this dataset for EDA
quick_preprocessed_dataset <- function() {
  
  # Set working directory (Modify is required)
  setwd("~/Documents/RStudio/PFDA Assignment")
  
  # Read the cleaned dataset
  dataset_cleaned <- read.csv("C:/Users/Lenovo/Desktop/APU/Degree in Computer Science (Specialist in Data Analyst)/Sem 1/PFDA/Assignment/cleaned_hacking_data.csv")
  
  # Libraries
  library(dplyr)
  library(ggplot2)
  
  # Change data type
  dataset_cleaned <- dataset_cleaned %>%
    mutate(
      Date = as.Date(Date),
      Country = factor(Country),
      WebServer = factor(WebServer),
      Continent = factor(Continent),
      OS_Family = factor(OS_Family)
    )
  
  # Return the dataset
  return (dataset_cleaned)
}

# ----------------------------------------------------------------------------
# Member 2: Eu Jun Hong (TP068580)
# ----------------------------------------------------------------------------

# Retrieve dataset
member_2_dataset <- quick_preprocessed_dataset()

## Check missing values (NA values)
colSums(is.na(member_2_dataset))

# ------------------------------------------------------------------------------
# Research Question 1: What is the relationship between OS family and loss?
# ------------------------------------------------------------------------------

## Create ONLY "OS" and "Loss" dataset
Q1_data <- member_2_dataset %>%                       # Create a data frame that is called "Q1_data"
  filter(!is.na(OS_Family)) %>%                       # Get "OS_Family" data that is not a missing value
  mutate(OS_Family = droplevels(OS_Family)) %>%       # Removes all unused levels in OS_Family 
  select(os_family = OS_Family, loss = Loss)          # Select OS_Family and Loss to display ONLY

## Summary Statistics
Q1_summary <- Q1_data %>%                             # Create a data frame called "Q1_summary"
  group_by(os_family) %>%                             # Group the data based on OS_Family
  summarise(                                          # Used to create summary statistics in data frame
    mean = mean(loss),                                # Calculate the mean / average of Loss
    variance = var(loss),                             #calculate the variance of Loss
    standard_deviation = sd(loss),                    # Calculate the standard deviation of Loss
    minimum = min(loss),                              # Search for the smallest number of loss
    number_of_OS = n()                                # Find the unique names of OS rows in OS_Family -> group by(os_family) 
  )

## Barplot
### Dataset Preparation
Q1_graph <- Q1_summary %>%                            # Create a data frame and call it "Q1_graph"
  arrange(mean) %>%                                   # Sort by average loss
  slice(1:10)                                         # Display ONLY first 10 OS

### Barplot Code
Q1_barplot <- barplot(Q1_graph$mean,                                        # Create a data frame for barplot <- Call a barplot function and use mean of loss
                      names.arg = Q1_graph$os_family,                       # Plot "OS_Family" specifically in barplot
                      main = "Average Operating System Loss (First 10 OS)", # Title of the barplot
                      xlab = "Loss",                                        # Label at x-axis
                      ylab = "Operating System",                            # Label at y-axis
                      xlim = c(0, 25),                                      # X-axis Grid with scale from 0 to 25
                      col = rainbow(10),                                    # Set the bars display in rainbow color
                      horiz = TRUE,                                         # Display barplot horizontally
                      las = 1,                                              # Make labels shows horizontally
                      cex.names = 0.6)                                      # "Operating System" word size

# Adjust text placement
text(x = Q1_graph$mean,                                     # Set x-axis as mean of loss
     y = Q1_barplot,                                        # Set y-axis as barplot
     labels = round(Q1_graph$mean, 4),                      # Round up average loss to 4 decimal places
     pos = 4,                                               # Display average loss to the right
     offset = 0.5,                                          # Distance between bars and text
     cex = 0.8)                                             # Reduce text size


# ------------------------------------------------------------------------------
# 2. Relationship Between Web Server Version and Loss
# ------------------------------------------------------------------------------

## Create ONLY "WebServer" and "Loss" dataset
Q2_data <- member_2_dataset %>%                             # Create "Q2_data" data frame
  filter(!is.na(WebServer)) %>%                             # Display "WebServer" that have values only
  mutate(WebServer = droplevels(WebServer)) %>%             # Remove all unused level in WebServer
  select(ws = WebServer, loss = Loss)                       # Display only "WebServer" and "Loss" column

## Summary statistics
Q2_summary <- Q2_data %>%                                   # Create "Q2_summary" data frame
  group_by(ws) %>%                                          # Group by "WebServer"
  summarise( # Create summary statistics
    mean = mean(loss),                                      # Calculate mean / average of loss
    variance = var(loss),                                   # Calculate variance of loss
    standard_deviation = sd(loss),                          # Calculate the standard deviation of loss
    minimum = min(loss),                                    # Search for the least loss
    number_of_WebServer = n() # Display unique names for WebServer
  )

## Barplot
### Dataset Preparation
Q2_data <- Q2_summary %>%                       # Create "Q2_data" data frame
  arrange(mean) %>%                             # Arrange "Q2_data" based on mean of loss ascendingly
  slice(1:10)                                   # Display only the first 10 of the data frame

### Barplot Code
barplot <- barplot(Q2_data$mean,                                         # Create "barplot" data frame <- use mean of loss from "Q2_data"
                   names.arg = Q2_data$ws,                               # Plot WebServer as x-axis in barplot
                   main = "Average WebServer Loss (First 10 WebServer)", # Title of barplot
                   xlab = "Loss",                                        # Label at x-axis
                   ylab = "Operating System",                            # Label at y-axis
                   xlim = c(0, 20),                                      # Display a-axis grid from 0 to 20
                   col = rainbow(10),                                    # Set bars to rainbow color
                   horiz = TRUE,                                         # Set barplot to display horizontally
                   las = 1,                                              # Set labels shows horizontally
                   cex.names = 0.6) #Adjust the size of y-axis label

### Text on bar's right side
text(x = Q2_data$mean,                    # Use text function <- plot mean of loss from "Q2_data" as x-axis
     y = barplot,                         # Plot y-axis as barplot
     labels = round(Q2_data$mean, 4),     # Round up the decimal place of mean to 4 decimal places
     pos = 4,                             # Display average loss on the right of the bar
     offset = 0.5,                        # Distance between bar and the text
     cex = 0.8)                           # Slightly reduce the size of text

# ------------------------------------------------------------------------------
# 3. Relationship Between OS and Web Server
# ------------------------------------------------------------------------------

## Create "OS", "WebServer" and "Loss" dataset
Q3_data <- member_2_dataset %>%                       # Create "Q3_data" data frame
  filter(!is.na(OS_Family)) %>%                       # Include "OS_Family" that have values only 
  mutate(OS_Family = droplevels(OS_Family)) %>%       # Removes all unused levels in OS_Family 
  filter(!is.na(WebServer)) %>%                       # Filter missing values out of "WebServer"
  mutate(WebServer = droplevels(WebServer)) %>%       # Removes all unused levels in WebServer
  select(os = OS_Family, ws = WebServer, loss = Loss) # Display the data frame containing "OS_Family", "WebServer" and "Loss"

# Heatmap
ggplot(Q3_data, aes(x = ws, y = os, fill = loss)) +            # Create a ggplot using "Q3_data" with "OS_Family", "WebServer" and "Loss" column
  geom_tile(color = "white", linewidth = 0.5) +                # Create a heatmap <- Add white borders between loss
  scale_fill_gradient2(low = "green", 
                       mid = "yellow", 
                       high = "red", 
                       midpoint = median(Q3_data$loss)) +      # fill loss with different colors
  labs(title = "Loss Heatmap by Webserver and OS",             # Title of the heatmap
       x = "Webserver",                                        # Label of x-axis
       y = "Operating System",                                 # Label of y-axis
       fill = "Loss") +                                        # Values within the heatmap
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1),  # Rotate x-axis labels to readability angle
    axis.text.y = element_text(size = 10),              # Adjust y-axis text size
    axis.title = element_text(size = 12),               # Adjust titles text size
    plot.title = element_text(hjust = 0.5)              # Center the plot title
  )
