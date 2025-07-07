# Import libraries
library(dplyr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HYPOTHESIS TESTING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import dataset from member 3
hypothesis_dataset <- read.csv("member_3_dataset.csv")

# Select relevant data
hypothesis_dataset <- hypothesis_dataset %>%
  select(
    OS_Family,
    WebServer,
    DownTime,
    Loss
  )

# ----------------------------------------------------------
# ANCOVA TESTING
# ----------------------------------------------------------

# Modify the number of lines to be printed (to show all results)
options(max.print = 10000)

# Perform ANCOVA testing (for categorical variables and downtime)
ancova_model <- aov(Loss ~ DownTime * WebServer * OS_Family, data = hypothesis_dataset)
summary(ancova_model)

# ----------------------------------------------------------
# LINEAR MODEL FOR TESTING
# ----------------------------------------------------------

# Linear model combined with different variables
linear_model <- lm(Loss ~ DownTime * WebServer * OS_Family, data = hypothesis_dataset)
summary(linear_model)

# ----------------------------------------------------------
# PERCENTAGE CALCULATION
# ----------------------------------------------------------

# Formula (as derived in report)
percentage_change <- function(initial_point_in_transformed) {
  return (1 - (1 - 5.804 / (2 * initial_point_in_transformed + 5.804)) ^ 4.5)
}

# Plot a graph for all possible values in dataset
x_values <- hypothesis_dataset %>%
  filter(OS_Family == "Linux", WebServer == "Apache") %>%
  select("Loss")

# Generate a data frame to store the data
data_for_graph <- data.frame(
  Loss = x_values,
  Percentage = sapply(x_values, percentage_change)
)

# Set correct column names if necessary
colnames(data_for_graph) <- c("Loss", "Percentage")

# Draw the graph
plot(x = data_for_graph$Loss,
     y = data_for_graph$Percentage * 100,
     main = "Percentage Decrease of Loss",
     xlab = "Loss (after transformation and after reduction)",
     ylab = "Percentage (%)")

# Print results
cat("Minimum percentage:",
    min(data_for_graph$Percentage) * 100, "%\n",
    "Maximum percentage:",
    max(data_for_graph$Percentage) * 100, "%\n")

# ----------------------------------------------------------
# UNUSED TESTING
# ----------------------------------------------------------

# # Hypothesis 1: OS Family vs Loss (ANOVA)
# anova_os <- aov(Loss ~ OS_Family, data = hypothesis_dataset)
# summary(anova_os) 
# 
# # Hypothesis 2: Web Server vs Loss (ANOVA)
# anova_webserver <- aov(Loss ~ WebServer, data = hypothesis_dataset)
# summary(anova_webserver)  # p-value check
# 
# # Hypothesis 3: Correlation between Downtime and Loss
# cor_downtime <- cor(hypothesis_dataset$DownTime, hypothesis_dataset$Loss, method = "spearman")
# print(cor_downtime)  # Spearman correlation coefficient & p-value
# 
# # Hypothesis 4: Protocol Type on Loss (t-test)
# t_test_protocol <- t.test(Loss ~ Protocol_Type, data = hypothesis_dataset)
# print(t_test_protocol)  # t-test result
# 
# # Hypothesis 5: File Type vs Loss (ANOVA)
# anova_file_type <- aov(Loss ~ URL_File_Type, data = hypothesis_dataset)
# summary(anova_file_type)  # p-value check
# 
# # Hypothesis 6: Sector vs Loss (ANOVA)
# anova_sector <- aov(Loss ~ Sector, data = hypothesis_dataset)
# summary(anova_sector)  # p-value check
# 
# # Hypothesis 7: IP Class vs Loss (ANOVA)
# anova_IP_Class <- aov(Loss ~ IP_Class, data = hypothesis_dataset)
# summary(anova_IP_Class)  # p-value check

