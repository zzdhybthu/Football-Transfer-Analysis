source('__init__.R')
source('utils.R')

df <- final_df_with_gpt_3.5
# df <- filter(df, !is.na(df$fact.url))
df <- downSample(df)
df$y <- as.numeric(df$fact.real_rumor) - 1
df$GPT.confidence <- as.numeric(df$GPT.confidence)
df$Rumor.Sentiment <- as.numeric(df$Rumor.Sentiment)

correlation <- data.frame(
  Variable = c("GPT.confidence", "Rumor.Sentiment"),
  Correlation = c(cor(df$GPT.confidence, df$y, use = "complete.obs"),
                  cor(df$Rumor.Sentiment, df$y, use = "complete.obs"))
)
print(correlation)


# Filter out data with confidence values of -1, -0.5, 0, 0.5, 1
df <- df %>% filter(GPT.confidence %in% c(-1, -0.5, 0, 0.5, 1))

# Prepare first dataset (GPT.confidence vs Mean y)
grouped_data1 <- df %>%
  group_by(GPT.confidence) %>%
  summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop") %>%
  mutate(Plot_Group = "GPT.confidence", x_label = as.character(GPT.confidence))

# Prepare second dataset (Rumor.Sentiment_Group vs Mean y)
grouped_data2 <- df %>%
  mutate(Rumor.Sentiment_Group = cut(
    Rumor.Sentiment,
    breaks = seq(
      min(Rumor.Sentiment, na.rm = TRUE),
      max(Rumor.Sentiment, na.rm = TRUE), 
      length.out = 6
    ),
    include.lowest = TRUE,
    labels = FALSE
  )) %>%
  group_by(Rumor.Sentiment_Group) %>%
  summarise(
    mean_y = mean(y, na.rm = TRUE),
    min_sentiment = min(Rumor.Sentiment, na.rm = TRUE),
    max_sentiment = max(Rumor.Sentiment, na.rm = TRUE),
    Sentiment_Range = paste0(round(min_sentiment, 2), " - ", round(max_sentiment, 2)),
    .groups = "drop"
  ) %>%
  arrange(min_sentiment) %>%
  mutate(Plot_Group = "Rumor.Sentiment", x_label = Sentiment_Range)

# Combine both datasets
combined_data <- bind_rows(
  grouped_data1,
  grouped_data2
)

# Plot combined data using facet_wrap
ggplot(combined_data, aes(x = reorder(x_label, as.numeric(min_sentiment)), y = mean_y, fill = Plot_Group)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_point(aes(group = Plot_Group), size = 3, color = "darkred") +
  geom_line(aes(group = Plot_Group), color = "darkblue", linetype = "dashed") +
  facet_wrap(~Plot_Group, scales = "free_x") +
  labs(
    title = "Comparative Analysis: GPT.confidence and Rumor Sentiment vs Average Rumor Accuracy",
    x = NULL,
    y = "Average y"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(linetype = "dotted"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
