# Clear Environment
rm(list=ls())

# Import necessary libraries
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)

# 1 - Data preparation and Loading

# Import dataset using read.csv & the dropbox link
countypres <- read.csv(paste0(
    "https://www.dropbox.com/scl/fi/9d6ctufur3g72nfdtneea/",
    "countypres_2000-2020.csv?rlkey=vm0mtz12wgh6qxsgf7ops6pjx&dl=1"
))
# Print the first 6 rows of the dataset
head(countypres)
# Look at structure of the dataset 
str(countypres)
# Variable names 
names(countypres)

# Filter the dataset to just focus on the 2020 election data
countypres_2020 <- countypres %>% 
filter(year == 2020) # will filter the data to only include the 2020 election data

#Summarise election results at the state level
state_results <- countypres_2020 %>% # Create a new dataframe for state results
    group_by(state, party, candidate) %>% # Group by state, party and candidate
    summarise(candidate_votes = sum(candidatevotes, na.rm = TRUE)) # We are summing up the candidate votes in each state 


# Total Votes for each state 
state_total_votes <- countypres_2020 %>% 
    group_by(state) %>% 
    summarise(total_state_votes = sum(candidatevotes, na.rm = TRUE))

# Merge the two dataframes
state_results <- state_results %>% 
    left_join(state_total_votes, by = "state")

# Party vote share
state_results <- state_results %>% 
    mutate(party_vote_share = candidate_votes * 100 / total_state_votes)

# Winning candidate in each state 
state_winners <- state_results %>% 
    group_by(state) %>% 
    filter(candidate_votes == max(candidate_votes)) %>% 
    select(state, party, candidate, candidate_votes, party_vote_share)


# Find some interesting insights from the data frames 
# State with the highest voter turnout
state_with_highest_turnout <- state_total_votes %>% 
    filter(total_state_votes == max(total_state_votes))

print(state_with_highest_turnout) # California has the highest voter turnout in the 2020 election

# State with the highest Republican vote share
republican_share <- state_results %>% 
    filter(party == "REPUBLICAN") %>%
    arrange(desc(party_vote_share))

print(republican_share) # Wyoming has the highest Republican vote share in the 2020 election with 62.03%
tail(republican_share) # The state with the lowest Republican vote share District of Columbia with 5.39%

# State with the highest Democratic vote share
democrat_share <- state_results %>% 
    filter(party == "DEMOCRAT") %>%
    arrange(desc(party_vote_share))

# 2 
# State-Level Visulisation 

# Create a map at State Level of the winners of the 2020 election using map_data()
us_map <- map_data("state")

# Prepare state_results for merging with map data
state_winners <- state_winners %>%
    mutate(region = tolower(state))

# Merge map data with state results
map_data <- us_map %>%
    left_join(state_winners, by = "region")

# Plot the map

# Calculate centroids for each state
state_centroids <- map_data %>%
    group_by(state) %>%
    summarize(long = mean(long), lat = mean(lat))

# Plot the map
p <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = party)) +
    geom_polygon(color = "white") +
    scale_fill_manual(values = c("DEMOCRAT" = "cornflowerblue", "REPUBLICAN" = "red")) +
    theme_void() +
    labs(title = "2020 US Presidential Election Results by State",
         fill = "Party") +
    theme(
        legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 12),   # Increase legend text size
        plot.title = element_text(hjust = 0.5, size = 20)  # Center and increase title size
    ) +
    geom_text(data = state_centroids,
              aes(x = long, y = lat, label = state),
              inherit.aes = FALSE,  # Avoid inheriting global aesthetics like `group`
              color = "black", size = 3, fontface = "bold") +
    coord_fixed(ratio = 1.5)  # Adjust the ratio to make the image wider or taller

# Save the plot
ggsave("us_election_results.png", p, width = 10, height = 6, units = "in")


# Republican Vote Share Map 
p <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = party_vote_share)) +
    geom_polygon(color = "black") +
    scale_fill_gradient(low = "lightyellow", high = "brown", limits = c(0, 70), name = "Republican Vote Share (%)") +
    theme_void() +
    labs(title = "2020 US Presidential Election Republican Vote Share by State") +
    theme(
        legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 12),   # Increase legend text size
        plot.title = element_text(hjust = 0.5, size = 20)  # Center and increase title size
    ) +
    geom_text(data = state_centroids,
              aes(x = long, y = lat, label = state),
              inherit.aes = FALSE,  # Avoid inheriting global aesthetics like `group`
              color = "black", size = 4, fontface = "bold") +
    coord_fixed(ratio = 1.5)  # Adjust the ratio to make the image wider or taller

# Save my plot
ggsave("us_election_republican.png", p, width = 10, height = 6, units = "in")

# 3 County-Level Visulisation (2000-2020)
# Map out the results from 2000-2020 at county level
countypres <- countypres %>%
    select(year, county_name, party, candidatevotes, totalvotes) %>%
    mutate(county_vote_shares = candidatevotes / totalvotes * 100) 

# Winning Candidate in each county
county_results <- countypres %>%
    group_by(year, county_name) %>%
    filter(candidatevotes == max(candidatevotes)) %>%
    select(year, county_name, party) %>%
    mutate(county_name = tolower(county_name))


# Merge map data with county results data frame
county_map_data <- map_data("county") %>%
    left_join(county_results, by = c("subregion" = "county_name")) %>%
    filter(!is.na(party))  # Remove rows with missing party information

# Plot the faceted map
f <-  ggplot(county_map_data, aes(x = long, y = lat, group = group, fill = party)) +
  geom_polygon(color = "black", size = 0.05) +
  scale_fill_manual(values = c("DEMOCRAT" = "cornflowerblue", "REPUBLICAN" = "red"), na.value = "grey50") +
  theme_void() +
  labs(title = "2000-2020 US Presidential Election Results by County",
       fill = "Party") +
  theme(
      legend.title = element_text(size = 14),  # Increase legend title size
      legend.text = element_text(size = 12),   # Increase legend text size
      plot.title = element_text(hjust = 0.5, size = 20)  # Center and increase title size
  ) +
  facet_wrap(~year, ncol = 3) +  # Spread over multiple rows, adjust the number of columns
  coord_fixed(1.3) +  # Aspect ratio for better proportion
  theme(panel.spacing = unit(0.5, "lines"))  # Add spacing between panels

names(county_map_data)


# 4 - Animated U.S. Election Map (2000-2020)
# Create an animated map of the U.S. election results from 2000 to 2020


# Create the animated map
animated_map <- ggplot(county_map_data, aes(x = long, y = lat, group = group, fill = party)) +
    geom_polygon(color = "black", size = 0.05) +
    scale_fill_manual(values = c("DEMOCRAT" = "cornflowerblue", "REPUBLICAN" = "red"), na.value = "grey50") +
    theme_void() +
    labs(title = "US Presidential Election Results by County (2000-2020)",
         fill = "Party") +
    theme(
        legend.title = element_text(size = 15),  # Increase legend title size
        legend.text = element_text(size = 12),   # Increase legend text size
        plot.title = element_text(hjust = 0.5, size = 15)  # Center and increase title size
    ) +
    transition_states(year, transition_length = 5, state_length = 5) +
    enter_fade() +
    exit_fade() +
    coord_fixed(1.3) +  # Aspect ratio for better proportion
    theme(panel.spacing = unit(0.5, "lines")) +  # Add spacing between panels
    ggtitle('US Presidential Election Results by County: {closest_state}')  # Dynamic title with year

# Animate and save the gif
animate(animated_map, nframes = 100, fps = 20, duration = 5, renderer = gifski_renderer("us_election_result.gif"))
    

