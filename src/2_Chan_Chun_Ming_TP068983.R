# ----------------------------------------------------------------------------
# QUICK PREPROCESSING CLEANED DATASET
# ----------------------------------------------------------------------------

# Define a function to retrieve the cleaned dataset -> everyone will be using this dataset for EDA
quick_preprocessed_dataset <- function() {
  
  # Set working directory (Modify if required)
  setwd("C:\\Users\\User\\OneDrive - Asia Pacific University\\Degree Year 2\\Semester 1\\Programming for Data Analysis\\PFDA Assignment\\AssignmentWD")
  
  # Load libraries (again)
  library(dplyr)
  
  # Read the cleaned dataset
  dataset_cleaned <- read.csv("cleaned_hacking_data.csv")
  
  # Change data type
  dataset_cleaned <- dataset_cleaned %>%
    mutate(
      Country = factor(Country),
      WebServer = factor(WebServer),
      Continent = factor(Continent),
      OS_Family = factor(OS_Family)
    )
  
  # Return the dataset
  return (dataset_cleaned)
}

# ------------------------------------------------------------------------------
# Member 1: Chan Chun Ming (TP068983)
# ------------------------------------------------------------------------------

# Load required library
library(tidyverse) #For data manipulation
library(dplyr) #For data cleaning
library(VIM) #kNN to predict missing value
library(ggplot2) #To visualize findings
library(reshape2) #To aggregate data

# Retrieve dataset
member_1_dataset <- quick_preprocessed_dataset()

#Check number of records for each unique OS_Family
count_OS_label <- member_1_dataset %>%
  count(OS_Family)
count_OS_label

#Use kNN to predict the missing value for OS_Family
member_1_dataset <- kNN(member_1_dataset, 
                        variable = "OS_Family", 
                        k = 5)                      # Use the nearest 5 similar neighbours


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Research Question 1: How does OS family, downtime and year correlate with revenue loss?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Build a multiple regression model
lm_loss <- lm(Loss ~ DownTime + OS_Family + Year, data = member_1_dataset)
summary(lm_loss)


#Scatter plot for downtime vs loss
ggplot(member_1_dataset, aes(x = DownTime, y = Loss)) +
  geom_point() +
  labs(
    title = "Scatter plot of downtime vs loss",
    xlab = "Downtime",
    ylab = "Loss"
  ) +
  geom_smooth(method = "lm", col = "red") +              #draw a regression line
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#Check the correlation between downtime and loss
cor(member_1_dataset$DownTime, member_1_dataset$Loss, method = "pearson")


#Visualize Loss distribution for each OS Family
ggplot(member_1_dataset, aes(x = OS_Family, y = Loss, fill = OS_Family)) +
  geom_boxplot(outlier.color = "red") +                  #Visualize if any massive outliers
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Loss Distribution by OS Family",
       x = "OS Family",
       y = "Loss") +
  theme(plot.title = element_text(hjust = 0.5))


#Visualize trend for loss across years
ggplot(member_1_dataset, aes(x = Year, y = Loss, group = 1)) +
  
  #show trend for average loss over years
  geom_line(stat="summary", fun="mean", color="lightblue", linewidth=1.5) + 
  geom_point(stat="summary", fun="mean", color="navyblue", size=2) +
  
  #draw regression line w/o shaded Confidence Interval
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) + 
  labs(
    title = "Average Loss Over Year",
    x = "Year",
    y = "Average Loss"
  ) +
  scale_y_continuous(expand = c(0, 0.5)) +                #expand y axis label
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Group mean Loss per Year and OS_Family
heatmap_data <- aggregate(
  Loss ~ Year + OS_Family, 
  data = member_1_dataset, 
  FUN = "mean"
  )

# Heatmap Plot (Combination for year and os towards loss)
ggplot(heatmap_data, aes(x = Year, y = OS_Family, fill = Loss)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightgreen",
                       high = "red",
                       mid = "white",
                       midpoint = median(heatmap_data$Loss)
                       ) +
  labs(
    title = "Heatmap of Loss by Year and OS Family",
    x = "Year", y = "OS Family",
    fill = "Avg Loss"
    ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Research Question 2: What is the proportion of each OS family that contributed 
# to revenue loss due to web defacement attacks over the years?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregate total loss for each OS Family per Year
loss_proportion <- member_1_dataset %>%
  group_by(Year, OS_Family) %>%
  summarise(
    Total_Loss = sum(Loss)
    ) %>%
  mutate(
    Proportion = Total_Loss / sum(Total_Loss)                   # Calculate proportion for each OS Family
    )  

#Stacked Bar to check the proportion for each OS Family
ggplot(loss_proportion, aes(x = Year, y = Proportion, fill = OS_Family)) +
  geom_bar(stat = "identity", position = "fill") +              #Stacked-bar
  scale_y_continuous(labels = scales::percent) +                #Convert the scale into proportion for frequency
  labs(
    title = "Proportion of Loss by OS Family Over Years",
    x = "Year", 
    y = "Total Loss"
    ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Research Question 3: What is the trend of loss for each OS Family across years 
# and which OS Family improves the most?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Visualize each OS family trends for loss across year
ggplot(heatmap_data, aes(x = Year, y = Loss, color = OS_Family, group = 1)) +
  geom_line(stat = "summary", fun = "mean", size = 1) +        #plot line based on average loss
  facet_wrap(~OS_Family) +                                     #Split each OS Family
  labs(
    title = "Loss Trend for Each OS Family Over Years",
    x = "Year", 
    y = "Avg Loss"
    ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +   #Slightly rotate the axis label
  theme(plot.title = element_text(hjust = 0.5))


# Categorize years into early (1998-2005) and late (2006-2015)
RQ3_m1 <- member_1_dataset %>%
  mutate(Year = as.numeric(as.character(Year)),                # Convert Year from factor to numeric
         Period = case_when(
           Year >= 1998 & Year <= 2005 ~ "1998-2005",
           Year >= 2006 & Year <= 2015 ~ "2006-2015",
           TRUE ~ NA_character_  # Catch unexpected values
         )) %>%
  group_by(OS_Family, Period) %>%
  summarize(Avg_Loss = mean(Loss), .groups = "drop")


#Bar plot to compare early years and late years for each OS family
ggplot(RQ3_m1, aes(x = OS_Family, y = Avg_Loss, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loss Comparison: Which OS Family Improves the most?",
       x = "OS Family",
       y = "Average Loss",
       fill = "Period") +
  
  #Zoom the data within this 18.5 - 20.5 range for comparison
  coord_cartesian(ylim = c(18.5, 20.5)) +     
  theme_minimal()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Research Question 4: How OS families react differently to loss when correlated with downtime?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Scatter Plot for Loss vs. Downtime of each OS Family
ggplot(member_1_dataset, aes(x = DownTime, y = Loss)) +
  geom_point(aes(color = OS_Family),
             alpha = 0.6) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x, 
    se = FALSE,                                #Ignore shaded CI area
    color = "black"
    ) +
  facet_wrap(~ OS_Family) +                    #Group by OS family
  labs(
    title = "Impact of Downtime on Financial Loss for Each OS Family",
    x = "Downtime Duration", 
    y = "Financial Loss"
    ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~