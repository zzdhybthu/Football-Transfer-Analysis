source('__init__.R')
source('utils.R')

df <- final_df_with_gpt_3.5
# df <- filter(df, !is.na(df$fact.url))
df <- downSample(df)
df$y <- as.numeric(df$fact.real_rumor) - 1

df$Month <- substr(df$Rumor.FormatDate, 5, 6)
df$Day <- substr(df$Rumor.FormatDate, 7, 8)
df$Hour <- substr(df$Rumor.FormatDate, 9, 10)
df$Minute <- substr(df$Rumor.FormatDate, 11, 12)

df$Month <- as.numeric(df$Month)
df$Day <- as.numeric(df$Day)
df$Hour <- as.numeric(df$Hour)
df$Minute <- as.numeric(df$Minute)

df_long <- df %>%
  mutate(Month = as.factor(Month),
         Day = as.factor(Day),
         Hour = as.factor(Hour),
         Minute = as.factor(Minute)) %>%
  pivot_longer(cols = c("Month", "Day", "Hour", "Minute"), 
               names_to = "TimePart", 
               values_to = "Value")

ggplot(df_long, aes(x = Value, y = y, fill = TimePart)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.shape = 21) +
  facet_wrap(~ TimePart, scales = "free_x", nrow = 2) +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    legend.position = "none" 
  ) +
  labs(
    x = "Time Component Values",
    y = "y",
    title = "Boxplots of Time Components"
  )




df$y <- as.factor(df$y)
model <- randomForest(y ~ Month + Day + Hour + Minute, data = df, importance = TRUE)

importance_df <- as.data.frame(importance(model)) %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, MeanDecreaseAccuracy = MeanDecreaseAccuracy) %>%
  arrange(desc(MeanDecreaseAccuracy))

ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy, fill = Feature)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Feature Importance Based on Random Forest Model",
    x = "Features",
    y = "Increase in Accuracy"
  )
