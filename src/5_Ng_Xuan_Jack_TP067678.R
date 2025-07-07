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
      Country = factor(Country),
      WebServer = factor(WebServer),
      Continent = factor(Continent),
      OS_Family = factor(OS_Family)
    )
  
  # Return the dataset
  return (dataset_cleaned)
}

---------------------------------------------------------------------------------------------------------------------------
#ANALYSIS - I have in view on how Year and Country affects Downtime due to web defacement attacks.

# RQ1 - What patterns / trends can be observed via average Loss caused by web defacement attacks over the Years?

###### LINE PLOT ######

# Install & apply data manipulation technique (dplyr)
library(dplyr)

# Group .csv data as data
data = read.csv("cleaned_hacking_data.csv")

# Get Date from .csv data and transform it to Year
data$Date = as.Date(data$Date)
data$Year = format(data$Date, "%Y")

# Group the dataset (data) based on Year
yearly_trends = data %>%
  group_by(Year) %>%
  
  # Calculates Loss & ignore missing values
  summarise(Average_Loss = mean(Loss, na.rm = TRUE))

# Converts Year into numerical-based column for chronological order
yearly_trends$Year = as.numeric(yearly_trends$Year)

# Plotting the line plot
plot(yearly_trends$Year, yearly_trends$Average_Loss, type = "o", 
     pch = 21, bg = "darkblue", cex = 1.5, lwd = 1.5,
     xlab = "Year", ylab = "Average Loss ($)", 
     main = "Trends in Average Loss Due to Web Defacement Attacks")
grid()

---------------------------------------------------------------------------------------------------------------------------
#	RQ2 - What were the differences existing in average Loss caused by web defacement attacks across various Countries?
# Install & apply visualization (rworldmap), chromatic dispersion (rColorBrewer), mapping and statistical analysis (classInt) and data manipulation technique (dplyr)
  library(rworldmap)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

# Group .csv data as data
data = read.csv("cleaned_hacking_data.csv")

# Ensure the correct column name for Loss values
loss_column = "Total_Loss"
if (!(loss_column %in% colnames(data))) {
  possible_names = c("Loss", "Defacement_Loss", "Attack_Loss", "Financial_Loss")
  found_name = possible_names[possible_names %in% colnames(data)]
  
  if (length(found_name) > 0) {
    cat("Using alternative column:", found_name, "\n")
    colnames(data)[colnames(data) == found_name] = loss_column
  } else {
    stop("Error: No suitable column found for loss values. Please check your dataset.")
  }
}

######  BAR PLOT ######

# Aggregate average Loss per country
avg_loss = data %>%
  group_by(Country) %>%
  summarise(Average_Loss = mean(Total_Loss, na.rm = TRUE))

# List only the top 20 countries by average Loss
top_20_countries = avg_loss %>%
  arrange(desc(Average_Loss)) %>%
  head(20)

# Generate barplot, list x, y and title, fill in color 
ggplot(top_20_countries, aes(x = reorder(Country, Average_Loss), y = Average_Loss, fill = Average_Loss)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Top 20 Countries by Average Loss Due to Web Defacement",
       x = "Country", y = "Average Loss", fill = "Avg_Loss") + theme_minimal()

###### WORLD MAP PLOT ######

# Group countrydata as worldMap
worldMap = joinCountryData2Map(avg_loss,
                               joinCode = "NAME",
                               nameJoinColumn = "Country")

# Assign colour to countries (Yellow – Orange - Red)
num_colors = 7
breaks = classIntervals(worldMap$Average_Loss, n = num_colors, style = "quantile")$brks
color_palette = brewer.pal(num_colors, "YlOrRd")  

# Plotting world map
mapParams = mapCountryData(worldMap,
                           nameColumnToPlot = "Average_Loss",
                           mapTitle = "Average Loss Due to Web Defacement Worldwide",
                           catMethod = breaks,
                           colourPalette = color_palette, addLegend = TRUE)

---------------------------------------------------------------------------------------------------------------------------
#	RQ3 - Does relationship exist or associated across Year, Country, and the average Loss due to web defacement attacks?
# Aggregate the average Loss of Countries per Year
yearly_country_trends = data %>%
group_by(Year, Country) %>%
summarise(Average_Loss = mean(Loss, na.rm = TRUE)) %>%
ungroup()

# Calculate the top 10 Countries with the highest average Loss
top_10_countries = yearly_country_trends %>%
  group_by(Country) %>%
  summarise(Total_Avg_Loss = mean(Average_Loss, na.rm = TRUE)) %>%
  top_n(10, Total_Avg_Loss) %>%
  pull(Country)

# Limit to only include top 10 Countries
filtered_data = yearly_country_trends %>%
  filter(Country %in% top_10_countries)

# Convert Year to numerical 
filtered_data$Year = as.numeric(filtered_data$Year)

######  HEATMAP PLOT ######

# Plotting the heatmap
ggplot(filtered_data, aes(x = Year, y = reorder(Country, -Average_Loss), fill = Average_Loss)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heatmap of Countries' Average Loss by Year (Top 10)",
       x = "Year", y = "Country", fill = "Avg Loss ($)") +
  theme_minimal()


###### STACKED BAR PLOT ######

# Reuse the data compiled in heatmap, group as countries, analyse the top 10 
top_10_countries = data %>%
  group_by(Country) %>%
  summarise(Total_Attacks = n()) %>%
  top_n(10, Total_Attacks) %>%
  pull(Country)

# Clean data and extract only the top 10
filtered_data = data %>% filter(Country %in% top_10_countries)

# Plotting the stacked bar plot
ggplot(filtered_data, aes(x = Year, fill = Country)) +
  geom_bar(position = "stack") +
  labs(title = "Number of Web Defacement Incidents Per Year (Top 10 Countries)",
       x = "Year", y = "Number of Attacks", fill = "Country") +
  theme_minimal()


