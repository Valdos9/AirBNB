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
  #Complete ID maintained
  listing_id = listings_raw$id,
  #Parses Listing date in YYYY-MM-DD
  snapshot_date = listings_raw$last_scraped |> 
    ymd(),
  #Squishes the neighborhood names to remove surrounding whitespace and reduce whitespace between words
  neighborhood = listings_raw$neighbourhood_cleansed |> 
    str_squish(),
  #Squishes the listings types, turns the letters into lowercase, and as an unordered factor
  room_type = listings_raw$room_type |> 
    str_squish() |> 
    str_to_lower() |> 
    factor(
      levels = c(
        "private room",
        "entire home/apt",
        "shared room",
        "hotel room"
      ),
      ordered = FALSE
    ),
  #Parses the capacity into ints
  accomodates = listings_raw$accommodates |> 
    as.numeric(),
  accomodates = if_else(accomodates <= 0, NA, accomodates),
  #Parses the price into ints
  price_usd = listings_raw$price |> 
    str_extract("\\d+\\.\\d+") |> 
    as.numeric(),
  price_usd = if_else(price_usd <= 0, NA, price_usd),
  #Parses the nights into ints
  minimum_nights = listings_raw$minimum_nights |> 
    as.numeric(),
  minimum_nights = if_else(minimum_nights <= 0, NA, minimum_nights),
  #Parses review date in YYYY-MM-DD
  last_review = listings_raw$last_review |> 
    ymd(),
  #Parses the number of reviews into ints
  number_of_reviews = listings_raw$number_of_reviews |> 
    as.numeric()
)

listings_clean <- listings_clean |> 
  mutate(
    days_since_review = as.numeric(snapshot_date - last_review),
    review_status = case_when(
      is.na(last_review) ~ "no review date",
      days_since_review >= 0 & days_since_review <= 180 ~ "recent",
      days_since_review > 180 ~ "older",
      .default = NA
    ) |> 
      factor(
        levels = c(
          "recent",
          "older",
          "no review date"
        ),
        ordered = FALSE
      )
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
