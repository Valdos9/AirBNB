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
  accommodates = listings_raw$accommodates |> 
    as.numeric(),
  #Parses the price into ints
  price_usd = listings_raw$price |> 
    str_extract("\\d+\\.\\d+") |> 
    as.numeric(),
  #Parses the nights into ints
  minimum_nights = listings_raw$minimum_nights |> 
    as.numeric(),
  #Parses review date in YYYY-MM-DD
  last_review = listings_raw$last_review |> 
    ymd(),
  #Parses the number of reviews into ints
  number_of_reviews = listings_raw$number_of_reviews |> 
    as.numeric()
)

listings_clean <- listings_clean |> 
  mutate(
    accommodates = if_else(accommodates <= 0, NA, accommodates),
    price_usd = if_else(price_usd <= 0, NA, price_usd),
    minimum_nights = if_else(minimum_nights <= 0, NA, minimum_nights),
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
###
#This sections is to check for any future review dates
future_reviews <- listings_clean |>
  filter(days_since_review < 0) |>
  select(listing_id, snapshot_date, last_review, days_since_review)

future_reviews

nrow(future_reviews)

complete_data <- listings_clean |> 
  filter(
    !is.na(price_usd),
    !is.na(accommodates),
    !is.na(minimum_nights),
    !is.na(room_type),
    !is.na(snapshot_date))

room_data <- complete_data |>
  filter(room_type == "private room" | room_type == "entire home/apt")

capacity_data <- room_data |>
  filter(accommodates >= 1, accommodates <= 6)

stay_data <- capacity_data |>
  filter(minimum_nights >= 1, minimum_nights <= 7)

analysis_data <- stay_data |>
  filter(price_usd > 0, price_usd <= 500) |>
  mutate(
    room_type = droplevels(room_type),
    room_type = relevel(room_type, ref = "private room")
  ) |>
  arrange(as.character(listing_id))

cleaning_audit <- tibble(
  raw_rows <- nrow(listings_raw),
  duplicate_rows <- raw_rows - nrow(distinct(listings_raw)),
  clean_rows <- nrow(listings_clean),
  missing_price <- sum(is.na(listings_clean$price_usd)),
  missing_capacity <- sum(is.na(listings_clean$accommodates)),
  missing_minimum_nights <- sum(is.na(listings_clean$minimum_nights)),
  missing_review_date <- sum(is.na(listings_clean$last_review)),
  complete_rows <- nrow(complete_data),
  room_rows <- nrow(room_data),
  capacity_rows <- nrow(capacity_data),
  stay_rows <- nrow(stay_data),
  analysis_rows <- nrow(analysis_data)
)
nrow(analysis_data)

n_distinct(analysis_data$listing_id)

sum(duplicated(analysis_data$listing_id))

range(analysis_data$snapshot_date, na.rm = TRUE)

range(analysis_data$last_review, na.rm = TRUE)

unique(analysis_data$room_type)

unique(analysis_data$review_status)

levels(analysis_data$room_type)

levels(analysis_data$review_status)

sum(analysis_data$days_since_review < 0, na.rm = TRUE)

summary(analysis_data$days_since_review)

sum(is.na(analysis_data$days_since_review))

summary(analysis_data$price_usd)
summary(analysis_data$accommodates)
summary(analysis_data$minimum_nights)

table(analysis_data$room_type)
table(analysis_data$review_status, useNA = "always")

glimpse(analysis_data)

# 2. Create room_summary, review_summary, plot_price, plot_room, plot_capacity.

room_summary <- analysis_data |>
  group_by(room_type) |>
  summarise(
    n = n(),
    mean_price = mean(price_usd),
    median_price = median(price_usd),
    sd_price = sd(price_usd),
    .groups = "drop"
  )

review_summary <- analysis_data |>
  group_by(review_status) |>
  summarise(
    n = n(),
    mean_price = mean(price_usd),
    .groups = "drop"
  )

plot_price <- ggplot(analysis_data, aes(x = price_usd)) +
  geom_histogram(
    binwidth = 25,
    fill = "steelblue",
    color = "black"
  ) +
  theme_minimal()

plot_room <- ggplot(analysis_data, aes(x = room_type, y = price_usd)) +
  geom_boxplot(
    fill = "steelblue"
  ) +
  coord_flip() +
  theme_minimal()

plot_capacity <- ggplot(
  analysis_data,
  aes(x = accommodates, y = price_usd)
) +
  geom_point(
    alpha = 0.4,
    color = "steelblue"
  ) +
  labs(
    x = "Guests",
    y = "USD per Night"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "red"
  ) +
  theme_minimal()


# 3. Create capacity_cor, capacity_model, capacity_terms, capacity_augmented,
# capacity_predictions, plot_residual.
capacity_cor <- cor(
  analysis_data$accommodates,
  analysis_data$price_usd,
  use = "complete.obs"
)

capacity_model <- lm(
  price_usd ~ accommodates,
  data = analysis_data
)

capacity_terms <- broom::tidy(capacity_model) |>
  select(term, estimate)

capacity_augmented <- broom::augment(capacity_model)

capacity_predictions <- data.frame(
  accommodates = c(2, 4, 6)
) |>
  mutate(
    predicted_price = predict(
      capacity_model,
      newdata = data.frame(
        accommodates = accommodates
      )
    )
  )


plot_residual <- ggplot(
  capacity_augmented,
  aes(x = .fitted, y = .resid)
) +
  geom_point(
    alpha = 0.4,
    color = "steelblue"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  theme_minimal()


# 4. Create room_model, room_terms and room_predictions.
# Reference group: private room. Verify predictions equal group means.
room_model <- lm(
  price_usd ~ room_type,
  data = analysis_data
)

summary(room_model)

analysis_data$room_type <- relevel(
  analysis_data$room_type,
  ref = "private room"
)

room_terms <- broom::tidy(room_model) |>
  select(term, estimate)

room_predictions <- data.frame(
  room_type = factor(
    c("private room", "entire home/apt"),
    levels = c("private room", "entire home/apt")
  )
) |>
  mutate(
    predicted_price = predict(
      room_model,
      newdata = data.frame(
        room_type = room_type
      )
    )
  )

room_comparison <- room_predictions |>
  left_join(
    room_summary |>
      select(room_type, mean_price),
    by = "room_type"
  ) |>
  mutate(
    difference = predicted_price - mean_price
  )




# 5. Create population_mean, sampling_means, sampling_summary, plot_sampling.
# set.seed(30322); all 1,000 n=20 draws first, then all 1,000 n=80 draws.
# Hint: replicate(1000, mean(sample(x, size=..., replace=FALSE)))
population_mean <- mean(analysis_data$price_usd)

set.seed(30322)

x <- analysis_data$price_usd

# Generate 1,000 sample means with sample size 20
means_20 <- replicate(
  1000,
  mean(sample(x, size = 20, replace = FALSE))
)

# Generate 1,000 sample means with sample size 80
means_80 <- replicate(
  1000,
  mean(sample(x, size = 80, replace = FALSE))
)

sampling_means <- tibble(
  sample_size = c(
    rep(20, 1000),
    rep(80, 1000)
  ),
  mean_price = c(
    means_20,
    means_80
  )
)

sampling_summary <- sampling_means |>
  group_by(sample_size) |>
  summarise(
    mean_of_means = mean(mean_price),
    sd_of_means = sd(mean_price),
    .groups = "drop"
  )
plot_sampling <- ggplot(
  sampling_means,
  aes(x = mean_price)
) +
  geom_histogram(
    binwidth = 5,
    fill = "steelblue",
    color = "white"
  ) +
  geom_vline(
    xintercept = population_mean,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  facet_wrap(
    ~ sample_size,
    ncol = 1
  ) +
  theme_minimal()



# Write interpretation and team verification in project2.Rmd.
