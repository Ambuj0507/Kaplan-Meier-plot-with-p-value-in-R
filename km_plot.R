# Load required libraries
library(survival)
library(ggplot2)
library(survminer)

# Creating a sample dataframe with 50 rows; using sample() to generate random numbers in R
df <- data.frame(
  ID = 1:50,
  OS_MONTHS = sample(1:50, 50, replace = TRUE),
  OS_STATUS = sample(c(0, 1), 50, replace = TRUE),
  DFS_MONTHS = sample(5:10, 50, replace = TRUE),
  DFS_STATUS = sample(c(0, 1), 50, replace = TRUE),
  GENE_SET_ALTERED = sample(c("Yes", "No"), 50, replace = TRUE)
)

#save dataframe
#write.csv(df,"your_data.csv",row.names = F)

# Load your data (replace 'your_data.csv' with your actual data file)
#df <- read.csv("your_data.csv")

# Prepare the survival object
os_surv <- Surv(df$OS_MONTHS, df$OS_STATUS)

# Fit the Kaplan-Meier model
os_surv_fit <- survfit(os_surv ~ GENE_SET_ALTERED, data = df)

# Create the Kaplan-Meier plot
km_plot <- ggsurvplot(
  os_surv_fit,
  data = df,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  legend.labs = c("Gene Set Altered", "Gene Set Not Altered"),
  xlab = "Months Survival",
  ylab = "% Surviving",
  ggtheme = theme_bw()
)

# Display the plot
print(km_plot)
