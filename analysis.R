# STT303 Project 2: Boston on a Budget
# Team ID: Osvaldo Perez-Eslava
# Members (2 or 3):
# Section leads draft; another member reviews. Submit one integrated script.
# Run from the project root. Do not edit the supplied CSV or use absolute paths.
library(tidyverse)
library(lubridate)
library(broom)

# 1. Create listings_raw, listings_clean, analysis_data and cleaning_audit.
# Import ALL columns as character (IDs exceed exact double-integer precision).
# Check/remove exact duplicates, normalize labels, and parse numbers/dates.
# Add days_since_review and review_status; missing review dates remain valid.
# Apply the exact sequential study-scope rules from the instructions.
# Hint: as.numeric(snapshot_date - last_review) gives a difference in days.
listings_raw <- read.csv(here::here("data", "boston_listings_raw.csv"), colClasses = 'character') |> distinct()
listings_clean <- tibble(
  listing_id = listings_raw$id,
  snapshot_date = listings_raw$last_scraped |> 
    ymd(),
  neighborhood = listings_raw$neighbourhood_cleansed |> 
    str_squish(),
  room_type = listings_raw$room_type |> 
    str_squish() |> 
    str_to_lower(),
  accomodates = listings_raw$accommodates,
  price_usd = listings_raw$price |> 
    str_extract("\\d+\\.\\d+"),
  minimum_nights = listings_raw$minimum_nights,
  last_review = listings_raw$last_review |> 
    ymd(),
  number_of_reviews = listings_raw$number_of_reviews
)
  

# 2. Create room_summary, review_summary, plot_price, plot_room, plot_capacity.

# 3. Create capacity_cor, capacity_model, capacity_terms, capacity_augmented,
# capacity_predictions, plot_residual.

# 4. Create room_model, room_terms and room_predictions.
# Reference group: private room. Verify predictions equal group means.

# 5. Create population_mean, sampling_means, sampling_summary, plot_sampling.
# set.seed(30322); all 1,000 n=20 draws first, then all 1,000 n=80 draws.
# Hint: replicate(1000, mean(sample(x, size=..., replace=FALSE)))

# Write interpretation and team verification in project2.Rmd.
