#STEP 1: DATA MANAGE
# 1. Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(lubridate)

# 2. Load Ratings Data (u.data)
ratings <- read_delim("u.data", 
                      delim = "\t", 
                      col_names = c("user_id", "item_id", "rating", "timestamp"),
                      col_types = "iiid") %>%
  mutate(timestamp = as_datetime(timestamp))

# 3. Load User Data (u.user)
users <- read_delim("u.user", 
                    delim = "|", 
                    col_names = c("user_id", "age", "gender", "occupation", "zip_code"),
                    col_types = "iiccc")

# 4. Load Movie Data (u.item)
genre_names <- c("unknown", "Action", "Adventure", "Animation", "Childrens", 
                 "Comedy", "Crime", "Documentary", "Drama", "Fantasy", 
                 "Film_Noir", "Horror", "Musical", "Mystery", "Romance", 
                 "Sci_Fi", "Thriller", "War", "Western")

movies <- read_delim("u.item", 
                     delim = "|", 
                     col_names = c("movie_id", "movie_title", "release_date", 
                                   "video_release_date", "IMDb_URL", genre_names),
                     locale = locale(encoding = "ISO-8859-1"),
                     show_col_types = FALSE) %>%
  mutate(release_date = dmy(release_date)) %>%
  select(-video_release_date)

# 5. Merge into one Master Dataframe
movie_lens_data <- ratings %>%
  left_join(movies, by = c("item_id" = "movie_id")) %>%
  left_join(users, by = "user_id")

# 6. Final Clean
movie_lens_data <- movie_lens_data %>% filter(!is.na(movie_title))
print("Data Cleaning Successful. Preview:")
glimpse(movie_lens_data)

# 1. Sort the data 
final_sorted_data <- movie_lens_data %>%
  arrange(user_id, timestamp)

# 2. Extract to a CSV file
write_csv(final_sorted_data, "cleaned_movielens_data.csv")

# 3. Confirmation message
print("File 'cleaned_movielens_data.csv' has been created successfully!")

getwd()

# STEP 2: DATA EXPLORE
# Install if not installed
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load your cleaned dataset
data <- read_excel("cleaned_movielens_data.xlsx")

# View structure
str(data)

num_users <- n_distinct(data$user_id)
num_users

num_movies <- n_distinct(data$item_id)
num_movies

avg_rating <- mean(data$rating)
avg_rating

popular_movies <- data %>%
  group_by(movie_title) %>%
  summarise(total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  slice(1:10)

popular_movies

active_users <- data %>%
  group_by(user_id) %>%
  summarise(total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  slice(1:10)

active_users

genre_columns <- c("Action","Adventure","Animation","Childrens","Comedy",
                   "Crime","Documentary","Drama","Fantasy","Film_Noir",
                   "Horror","Musical","Mystery","Romance","Sci_Fi",
                   "Thriller","War","Western")

genre_popularity <- colSums(data[genre_columns])

genre_popularity <- sort(genre_popularity, decreasing = TRUE)

genre_popularity

ggplot(data, aes(x = rating)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Rating Distribution",
       x = "Rating",
       y = "Count")

ggplot(popular_movies, aes(x = reorder(movie_title, total_ratings),
                           y = total_ratings)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Popular Movies",
       x = "Movie",
       y = "Number of Ratings")

genre_df <- data.frame(
  Genre = names(genre_popularity),
  Count = as.numeric(genre_popularity)
)

ggplot(genre_df[1:10, ], aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Genres",
       x = "Genre",
       y = "Count")


# STEP 3: DATA PRE-PROCESS
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)

data <- read_excel("cleaned_movielens_data.xlsx")

rating_data <- data %>% select(user_id, item_id, rating)
user_item_matrix <- rating_data %>% pivot_wider(names_from = item_id, values_from = rating, values_fill = 0)
user_item_matrix <- user_item_matrix %>% column_to_rownames(var = "user_id")

# calculate user means (excluding 0s)
user_means <- rating_data %>%
  group_by(user_id) %>%
  filter(rating > 0) %>%  # Exclude implicit 0s
  summarise(mean_rating = mean(rating))

# Join means back and normalize
normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating = ifelse(rating > 0, rating - mean_rating, 0))

# Recreate the normalized user-item matrix
normalized_user_item_matrix <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(names_from = item_id, values_from = normalized_rating, values_fill = 0) %>%
  column_to_rownames(var = "user_id")

head(user_item_matrix)
head (normalized_user_item_matrix)

# STEP 4: MODEL DEVELOPMENT
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("proxy")
install.packages("tibble")

library(readxl)
library(dplyr)
library(tidyr)
library(proxy)
library(tibble)

# 1 LOAD CLEANED DATASET
data <- read_excel("C:/Users/insyi/Downloads/cleaned_movielens_data.xlsx")

head(data)

# 2 SELECT IMPORTANT COLUMNS
rating_data <- data %>%
  select(user_id, item_id, rating)

head(rating_data)

# 3 CREATE USER ITEM MATRIX
user_item_matrix <- rating_data %>%
  pivot_wider(
    names_from = item_id,
    values_from = rating,
    values_fill = 0
  )

user_item_matrix <- user_item_matrix %>%
  column_to_rownames(var = "user_id")

head(user_item_matrix)

# 4 NORMALIZE RATINGS (REMOVE USER BIAS)
user_means <- rating_data %>%
  group_by(user_id) %>%
  filter(rating > 0) %>%
  summarise(mean_rating = mean(rating))

normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating =
           ifelse(rating > 0,
                  rating - mean_rating,
                  0))

# 5 CREATE NORMALIZED USER ITEM MATRIX
normalized_user_item_matrix <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(
    names_from = item_id,
    values_from = normalized_rating,
    values_fill = 0
  ) %>%
  column_to_rownames(var = "user_id")

head(normalized_user_item_matrix)

# 6 CONVERT TO MATRIX
matrix_data <- as.matrix(normalized_user_item_matrix)

# 7 CALCULATE MOVIE SIMILARITY (COSINE)
movie_similarity <- simil(
  t(matrix_data),
  method = "cosine"
)

movie_similarity_matrix <- as.matrix(movie_similarity)

# 8 VIEW SIMILARITY MATRIX
movie_similarity_matrix[1:10,1:10]

# 9 RECOMMENDATION FUNCTION
recommend_movies <- function(movie_id,
                             similarity_matrix,
                             top_n = 5){
  
  similarity_scores <- similarity_matrix[movie_id, ]
  
  sorted_scores <- sort(similarity_scores,
                        decreasing = TRUE)
  
  recommended_movies <- names(sorted_scores)[2:(top_n + 1)]
  
  return(recommended_movies)
}
  
# 10 GENERATE TOP 5 RECOMMENDATIONS
recommend_movies(50, movie_similarity_matrix)

# 11 OPTIONAL – SHOW TOP 10 SIMILAR MOVIES
similarity_scores <- movie_similarity_matrix[50, ]

sort(similarity_scores,
     decreasing = TRUE)[1:10]

# 12 CONVERT MOVIE IDS TO MOVIE TITLES
movie_lookup <- data %>%
  select(item_id, movie_title) %>%
  distinct()

# 13 GET RECOMMENDED MOVIE TITLES
recommended_ids <- recommend_movies(50, movie_similarity_matrix)

recommended_movies <- movie_lookup %>%
  filter(item_id %in% as.numeric(recommended_ids))

recommended_movies

# STEP 5: DASHBOARD DESIGN
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# 1. DATA CLEANING
data$release_date_clean <- parse_date_time(
  data$release_date, 
  orders = c("ymd", "dmy", "mdy")
)

if (any(is.na(data$release_date_clean))) {
  excel_dates <- as.Date(as.numeric(as.character(data$release_date)), origin = "1899-12-30")
  data$release_date_clean[is.na(data$release_date_clean)] <- excel_dates[is.na(data$release_date_clean)]
}

data$release_year <- year(data$release_date_clean)


# 2. AGE GROUP CREATION
data$age_group <- cut(
  data$age,
  breaks = c(0, 18, 25, 35, 50, 100),
  labels = c("0-18", "19-25", "26-35", "36-50", "50+"),
  include.lowest = TRUE
)

data$age_group <- factor(data$age_group, 
                         levels = c("0-18","19-25","26-35","36-50","50+"))

# 3. SCATTER PLOT 
ggplot(data, aes(x = age, y = rating, color = gender)) +
  geom_jitter(width = 0.8, height = 0.2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relationship Between Age and Rating",
       x = "Age",
       y = "Rating") +
  theme_minimal()

# 4. BOXPLOT
ggplot(data[!is.na(data$age_group), ], aes(x = age_group, y = rating)) +
  geom_boxplot(fill = "lightblue", 
               outlier.color = "red", 
               outlier.alpha = 0.5) +
  labs(title = "Rating Distribution Across Age Groups",
       x = "Age Group",
       y = "Rating") +
  theme_minimal()

# 6. RATING BY GENDER
ggplot(data, aes(x = rating, fill = gender)) +
  geom_histogram(position = "dodge", binwidth = 0.5) +
  labs(title = "Rating Distribution by Gender",
       x = "Rating",
       y = "Count") +
  theme_minimal()

# 8. GENRE POPULARITY
genre_columns <- c("Action","Adventure","Animation","Childrens","Comedy",
                   "Crime","Documentary","Drama","Fantasy","Film_Noir",
                   "Horror","Musical","Mystery","Romance","Sci_Fi",
                   "Thriller","War","Western")

genre_sums <- colSums(sapply(data[genre_columns], as.numeric), na.rm = TRUE)
genre_df <- data.frame(
  Genre = names(genre_sums),
  Count = as.numeric(genre_sums)
) %>% arrange(desc(Count))

ggplot(genre_df[1:10, ], aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Movie Genres",
       x = "Genre",
       y = "Count") +
  theme_minimal()

# 9. TOP 10 POPULAR MOVIES
popular_movies <- data %>%
  filter(!is.na(movie_title)) %>%
  group_by(movie_title) %>%
  summarise(total_ratings = n(), .groups = "drop") %>%
  slice_max(total_ratings, n = 10) 

ggplot(popular_movies, aes(x = reorder(movie_title, total_ratings), 
                           y = total_ratings)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Most Popular Movies",
       x = "Movie",
       y = "Number of Ratings") +
  theme_minimal()

# 10. STATISTICAL SUMMARY BY AGE GROUP
summary_stats <- data %>%
  filter(!is.na(age_group), !is.na(rating)) %>%
  group_by(age_group) %>%
  summarise(
    Mean_Rating   = mean(rating, na.rm = TRUE),
    Median_Rating = median(rating, na.rm = TRUE),
    SD_Rating     = sd(rating, na.rm = TRUE),
    IQR_Rating    = IQR(rating, na.rm = TRUE),
    Total_Count   = n(),
    .groups = "drop"
  )

print(summary_stats)

# 11. MEAN VS MEDIAN VISUALIZATION
summary_long <- summary_stats %>%
  select(age_group, Mean_Rating, Median_Rating) %>%
  tidyr::pivot_longer(cols = c(Mean_Rating, Median_Rating), 
                      names_to = "Metric", 
                      values_to = "Value")

ggplot(summary_long, aes(x = age_group, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Mean_Rating" = "#2c3e50", "Median_Rating" = "#18bc9c")) +
  labs(title = "Mean vs. Median Rating by Age Group",
       subtitle = "Differences indicate skewness in user reviews",
       x = "Age Group",
       y = "Rating Value") +
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal()

group_means <- data %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_rating = mean(rating, na.rm = TRUE)
  )

ggplot(data, aes(x = age, y = rating, color = age_group)) +
  geom_jitter(alpha = 0.2, width = 0.5, height = 0.2) +
  
  geom_point(data = group_means, 
             aes(x = mean_age, y = mean_rating), 
             color = "black", size = 5, shape = 18) +
  
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  
  # Styling
  scale_color_brewer(palette = "Set1") +
  labs(title = "Movie Ratings Grouped by Age Class",
       subtitle = "Diamonds represent the average (mean) for each age group",
       x = "Age",
       y = "Rating",
       color = "Age Group") +
  theme_minimal() +
  theme(legend.position = "bottom")

