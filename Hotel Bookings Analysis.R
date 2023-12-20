library(tidyverse)

hotel_bookings <-
  read.csv(
    "hotel_bookings.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )



## Cleaning & EDA
# Exploratory Data Analysis
dim(hotel_bookings) # Number of rows and columns
glimpse(hotel_bookings) # Data type of each column
summary(hotel_bookings) # Summary of the dataset
head(hotel_bookings) # First 6 rows
tail(hotel_bookings) # Last 6 rows

# Check duplicates and null values
sum(duplicated(hotel_bookings)) # Number of duplicated records
colSums(is.na(hotel_bookings)) # Number of null values per column

# Filtering out unwanted rows
clean_df <- hotel_bookings %>%
  filter(!(adults == 0 & children == 0 & babies == 0)) %>%
  select(-index,-company,-agent) %>%
  drop_na()

# Adjusting categorical values
clean_df <- clean_df %>%
  mutate(
    is_canceled = factor(if_else(
      is_canceled == 0, "not canceled", "canceled"
    )),
    is_repeated_guest = if_else(is_repeated_guest == 0, "not repeated", "repeated"),
    meal = recode(
      meal,
      "BB" = "bed and breakfast",
      "FB" = "full board",
      "HB" = "half board",
      "SC" = "self-catering"
    ),
  )

# Filtering specific meal types
clean_df <- clean_df %>%
  filter(meal %in% c("bed and breakfast", "full board", "half board", "self-catering"))

# Creating a new column for total number of spent nights
clean_df <- clean_df %>%
  mutate(total_nights = stays_in_weekend_nights + stays_in_week_nights)

# Converting 'arrival_date_month' to numeric
month_to_number <- function(month_name) {
  months_vec <- c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )
  return(match(month_name, months_vec))
}

clean_df$arrival_date_month <-
  factor(clean_df$arrival_date_month, levels = month.name)

clean_df <- clean_df %>%
  mutate(arrival_date_month_numeric = month_to_number(arrival_date_month))



## K-Means
# Selecting relevant columns for clustering
cluster_columns <- c(
  "lead_time",
  "total_nights",
  "adults",
  "children",
  "babies",
  "previous_cancellations",
  "previous_bookings_not_canceled",
  "booking_changes",
  "days_in_waiting_list",
  "adr"
)

# Creating a subset with selected columns
cluster_df <- clean_df[cluster_columns]

# Winsorize outliers in 'lead_time' column
cluster_df$lead_time <- ifelse(
  cluster_df$lead_time > quantile(cluster_df$lead_time, 0.95),
  quantile(cluster_df$lead_time, 0.95),
  ifelse(
    cluster_df$lead_time < quantile(cluster_df$lead_time, 0.05),
    quantile(cluster_df$lead_time, 0.05),
    cluster_df$lead_time
  )
)

# Scaling the data for K-means
scaled_data <- scale(cluster_df)

# Finding optimal number of clusters using Elbow Method
wss <- numeric(10)
for (i in 1:10) {
  kmeans_data <- kmeans(scaled_data, centers = i)
  wss[i] <- sum(kmeans_data$withinss)
}

plot(1:10,
     wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within Sum of Squares")

# Performing K-means clustering with the chosen number of clusters
num_clusters <- 5
set.seed(123)
kmeans_data <- kmeans(scaled_data, centers = num_clusters)

cluster_df$cluster <- as.factor(kmeans_data$cluster)
clean_df$cluster <- as.factor(kmeans_data$cluster)

# EDA on clusters
cluster_mean <- cluster_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

# On average:
# Cluster 1: High lead times
# Cluster 2: Two adults
# Cluster 3: Babies
# Cluster 4: Children
# Cluster 5: Single Adult

# Converting clusters to meaningful strings
clean_df <- clean_df %>%
  mutate(
    cluster = case_when(
      cluster == 1 ~ "With Babies",
      cluster == 2 ~ "Two Adults",
      cluster == 3 ~ "Early Bookers",
      cluster == 4 ~ "Single Adult",
      cluster == 5 ~ "With Children",
      TRUE ~ as.character(cluster)
    )
  )

cluster_mean <- cluster_mean %>%
  mutate(
    cluster = case_when(
      cluster == 1 ~ "Early Bookers",
      cluster == 2 ~ "Two Adults",
      cluster == 3 ~ "With Babies",
      cluster == 4 ~ "With Children",
      cluster == 5 ~ "Single Adult",
      TRUE ~ as.character(cluster)
    )
  )

# Calculating cancellation rates by cluster
cancellation_rates <- clean_df %>%
  group_by(cluster, is_canceled) %>%
  summarise(count = n()) %>%
  mutate(cancel_perc = count / sum(count))



## Decision Tree
# Load required library for decision tree
library(rpart)

# Defining columns for decision tree
tree_columns <- c(
  "lead_time",
  "total_nights",
  "adults",
  "children",
  "babies",
  "previous_cancellations",
  "previous_bookings_not_canceled",
  "booking_changes",
  "days_in_waiting_list",
  "adr",
  "is_canceled",
  "cluster"
)

# Creating a subset with selected columns
tree_df <- clean_df[tree_columns]

# Splitting the data into training and testing sets
train_index <-
  sample(1:nrow(tree_df), 0.7 * nrow(tree_df))
train_data <- tree_df[train_index, ]
test_data <- tree_df[-train_index, ]

# Fitting the decision tree model
tree_formula <- as.formula("train_data$is_canceled ~ .")
tree_model <-
  rpart(tree_formula, data = train_data, method = "class")

# Visualizing the decision tree
library(rpart.plot)
rpart.plot(tree_model)

# Making predictions on the test set
predictions <-
  predict(tree_model, newdata = test_data, type = "class")

# Evaluating model performance
accuracy <-
  sum(predictions == test_data$is_canceled) / nrow(test_data)



## Visualization
# Define UI for application
library(shiny)
ui <- fluidPage(
  titlePanel("Hotel Booking Analysis Dashboard"),
  
  sidebarLayout(sidebarPanel(),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel(
                      "Number of Bookings Per Month by Cluster",
                      plotOutput("bookings_per_month")
                    ),
                    
                    tabPanel(
                      "Cancellation Rates by Clusters",
                      plotOutput("cancellation_rates")
                    ),
                    tabPanel("Booking Changes by Cluster",
                             plotOutput("booking_changes")),
                    tabPanel("Average Lead Time by Cluster",
                             plotOutput("lead_times")),
                    tabPanel(
                      "Previous Cancellations vs Cancellation Ratio",
                      plotOutput("previous_cancellations")
                    )
                  )
                ))
)

server <- function(input, output) {
  output$bookings_per_month <- renderPlot(
    ggplot(clean_df, aes(x = arrival_date_month,
                         fill = cluster)) +
      geom_bar() +
      labs(
        title = "Number of Bookings Per Month by Cluster",
        x = "Month",
        y = "Number of Bookings",
        fill = "Cluster"
      ),
    
    res = 120
  )
  
  output$cancellation_rates <- renderPlot(
    ggplot(clean_df, aes(
      x = factor(cluster), fill = is_canceled
    )) +
      geom_bar(position = "fill") +
      labs(
        title = "Cancellation Rates by Clusters",
        x = "Cluster",
        y = "Cancellation Rate",
        fill = "Cancellation Status"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(),
    
    res = 120
  )
  
  output$booking_changes <- renderPlot(
    ggplot(clean_df, aes(x = booking_changes, fill = is_canceled)) +
      geom_bar(position = "dodge") +
      labs(
        title = "Cancellation Analysis by Booking Changes",
        x = "Booking Changes",
        y = "Count",
        fill = "Cancellation Status"
      ) +
      scale_x_continuous(limits = c(-0.5, 3)) +
      facet_wrap(~ cluster, scale = "free"),
    
    res = 120
  )
  
  output$lead_times <- renderPlot(
    ggplot(cluster_mean, aes(
      x = cluster, y = lead_time, fill = cluster
    )) +
      geom_bar(stat = "identity") +
      labs(
        title = "Average Lead Time by Cluster",
        x = "Cluster",
        y = "Lead Time",
        fill = "Cluster"
      ),
    
    res = 120
  )
  
  output$previous_cancellations <- renderPlot(
    ggplot(clean_df, aes(
      x = factor(previous_cancellations), fill = is_canceled
    )) +
      geom_bar(position = "fill") +
      labs(
        title = "Previous Cancellations vs. Cancellation Ratio",
        x = "Previous Cancellations",
        y = "Cancellation Ratio",
        fill = "Cancellation Status"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_x_discrete(limits = c("0", "1", "2", "3")),
    
    res = 120
  )
}

shinyApp(ui, server)
