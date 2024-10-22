## ECON0128

## Assignment 2

# Frisch Waugh Lovell Theorem

# Clear Environments    
rm(list=ls())

# Load necessary libraries
library(tidyverse)
library(texreg)


# Load in our dataset from dropbox  
county_data <- read.csv(paste0("https://www.dropbox.com/scl/fi/lumi2zlvbfcd2qsfnomqh/", 
"county_data_24.csv?rlkey=0jin81pffu9pohq6vquudbi1b&e=1&dl=1"))

# check if dataset has loaded in correctly
str(county_data)
head(county_data) 
names(county_data)


## Data Processing 
# Transform variable "y_hh_med" into its logaritmic form
county_data <- county_data %>% 
  mutate(log_y_hh_med = log(y_hh_med))

# Perform a multiple regression of rep_share on log_y_hh_med and white_share
model_lm <- lm(rep_share ~ log_y_hh_med + white_share, data = county_data)

# Summary of the model
screenreg(model_lm) 

## Application of FWL Theorem 
# Regress rep_share on white_share 
model_fwl1 <- lm(rep_share ~ white_share, data = county_data)

# Obtain the residuals 
res1 <- model_fwl1$residuals

# Regress log_y_hh_med on white_share
model_fwl2 <- lm(log_y_hh_med ~ white_share, data = county_data)

# Obtain the residuals
res2 <- model_fwl2$residuals

# Regress the residuals we have obtained 
model_fwl3 <- lm(res1 ~ res2)

screenreg(list(model_lm, model_fwl3))

# We can see that the coefficients of the model are the same as the original model

# 5 Visualising the FWL theorem

# Create a scatter plot of the residuals from the two regressions using ggplot2
ggplot(aes(x = res2, y = res1), data = county_data) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Plot of Residuals",
         x = "Residuals Model 2",
         y = "Residuals Model 1") +
    scale_x_continuous(limits = c(min(res2), max(res2))) +
    scale_y_continuous(limits = c(min(res1), max(res1))) +
    theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
    )

# The plot demonstrates the FWL theorem by showing the relationship between the two residuals 
# This is a negative relationship
# The slope should be equal to the coefficient of the log_y_hh_med variable in the original model

# 6 - Regress rep_share directly with res2 

model_fwl4 <- lm(rep_share ~ res2, data = county_data)

screenreg(list(model_lm, model_fwl3, model_fwl4))

# The coeffcienits of res2 in the model are the same as the coefficient of log_y_hh_med in the original model


## Task 2 

#Install necessary packages
install.packages("DBI")
install.packages("RPostgres")

# Load necessary libraries
library(DBI)
library(RPostgres)

# Connect to the Postgres database

data <- dbConnect(
  RPostgres::Postgres(),
  dbname = "movies_database",
  host = "34.89.96.247",
  port = "5432",
  user = "postgres",
  password = "UCL_ECON_0128"
)

# Check if the connection has been established
dbListTables(data)

dbListFields(data, "movies")

# Queries with dbplyr

# Filter the ratings table for records where the rating is 5 and the timestamp 
# is after January 1, 2016. Arrange the results by the timestamp
ratings <- tbl(data, "ratings")

ratings_filtered <- ratings %>%
  filter(rating == 5, timestamp > "2016-01-01") %>%
  arrange(timestamp) %>%
  show_query()

# Use collect() to execute the query and retrieve the results
collect(ratings_filtered)


