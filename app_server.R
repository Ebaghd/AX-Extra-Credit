library(ggplot2)
library(dplyr)
SP500_data <- read.csv("data/all_stocks_5yr.csv", stringsAsFactors = FALSE)
mean_data_plot_maker <- function(){
  mean_data <- SP500_data %>%
  filter(!is.na(open)) %>%
  filter(!is.na(high))%>%
  filter(!is.na(low)) %>%
  group_by(date = substr(date, 1, 4)) %>%
  summarize(open = mean(open),
            high = mean(high),
            low = mean(low),
            close = mean(close)) 
  market_time_values <- mean_data %>%
    select(open, high, low, close)
  Price_in_USD <- rowMeans(market_time_values)
  Year <- mean_data$date
  xandy <- data.frame(Price_in_USD, Year)
  ggplot(data = xandy) + 
  geom_col(mapping =
             aes(x = Year,
             y = Price_in_USD),
             fill = "blue") +
  ggtitle("Average price of S&P 500 Vs. Year")
}
mean_data_plot <- mean_data_plot_maker()