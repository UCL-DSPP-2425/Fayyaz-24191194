# Tutorial 4 
# Econ0128 
# Web Scraping


# Clear the environment
rm(list = ls())

# set the working directory
setwd("/Users/faze/Documents/MSc DSPP/Modules/ECON0128/Tutorial 4")

# Load necessary libraries for data manipulation, web scraping, functional programming, and file management
library(tidyverse)  # For data manipulation and piping
library(rvest)      # For web scraping HTML content
library(purrr)      # For functional programming (map functions)

# Define the base URLs for the Federal Reserve website
root_url <- "https://www.federalreserve.gov/"
main_url <- paste0(root_url, "monetarypolicy/fomchistorical2016.htm")  # URL of the main 2016 FOMC page

# Part 1: Extract all meeting minute links from the main page

# Read the main page's HTML content
webpage <- read_html(main_url)

# Use CSS selectors to locate all <a> tags with links to the meeting minutes pages
rel_paths <- webpage %>%
  html_elements("a") %>%          # Select all anchor tags
  html_attr("href") %>%           # Extract the 'href' attribute (URL) from each <a> tag
  str_subset("minutes") %>%       # Keep only URLs that contain "minutes" (relevant links)
  str_subset("htm")               # Further filter for ".htm" links (likely the correct pages)

# Define the section heading we need to locate within each meeting minutes page
heading_title <- "Staff Review of the Economic Situation"

# Part 2: Define a function to extract the relevant paragraphs within the targeted section

extract_paragraphs <- function(page) {
  # Extract text from all <p> (paragraph) elements on the page
  paragraphs <- page %>%
    html_elements("p") %>%        # Select all paragraph elements
    html_text(trim = TRUE)        # Get the text content and remove surrounding whitespace
  
  # Locate the start index of the section by finding the paragraph with the target heading
  start_index <- which(str_detect(paragraphs, heading_title))
  
  # Check if the heading was found; if not, return NA for this page
  if (length(start_index) == 0) return(NA)
  
  # Find the end index of the section by looking for the next empty paragraph or a paragraph starting with <strong>
  end_index <- start_index + which(str_detect(paragraphs[(start_index + 1):length(paragraphs)], "^\\s*$|^\\s*<strong>"))[1]
  
  # If no end index is found, default to the end of the paragraph list
  if (is.na(end_index)) end_index <- length(paragraphs)
  
  # Extract all paragraphs within the identified range for the section
  section_text <- paragraphs[start_index:(end_index - 1)]
  
  # Clean up extracted text by removing line breaks, carriage returns, and the heading title itself
  section_text <- section_text %>%
    str_remove_all("\r|\n|Staff Review of the Economic Situation") %>%  # Remove unwanted elements
    paste(collapse = " ")                                               # Combine paragraphs into a single text block
  
  # Return the cleaned text for this section
  section_text
}

# Part 3: Define a function to get the date and paragraphs for each meeting

get_meeting_data <- function(rel_path) {
  # Read the specific meeting minutes page based on the relative path
  page <- read_html(paste0(root_url, rel_path))
  
  # Extract the meeting date from the meta tag where 'twitter:title' holds the date information
  date <- page %>%
    html_elements("meta[name='twitter:title']") %>%  # Locate the meta tag with the 'twitter:title' attribute
    html_attr("content") %>%                         # Get the content attribute value (meeting date)
    str_remove("FOMC Minutes, ")                     # Remove "FOMC Minutes, " from the date for clean formatting
  
  # Use the extract_paragraphs function to get text from the "Staff Review of the Economic Situation" section
  text <- extract_paragraphs(page)
  
  # Return a tibble (data frame) with the extracted date and text for this meeting
  tibble(Date = date, Paragraphs = text)
}

# Part 4: Compile data for each meeting into a single data frame

# Use map_df() to apply the get_meeting_data function to each URL in rel_paths, 
# combining results into a single data frame (minutes_df)
minutes_df <- map_df(rel_paths, get_meeting_data)

# Part 5: Save the compiled data to a CSV file

# Write the final data frame to a CSV file in the working directory

write_csv(minutes_df, "minutes_df.csv")



