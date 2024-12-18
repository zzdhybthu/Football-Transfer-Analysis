source('__init__.R')
source('utils.R')

df <- final_df_with_gpt_3.5
df <- filter(df, !is.na(df$fact.url))
df <- downSample(df)
df$y <- as.factor(df$fact.real_rumor)
X_content <- textPrep(df$Rumor.Content, 0.9)

model <- randomForest(X_content, df$y, ntree = 100, importance = TRUE)
importance_df <- as.data.frame(importance(model)) %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, MeanDecreaseAccuracy = MeanDecreaseAccuracy) %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  head(20)

color_mapping <- c(
  "united" = "#fdbf6f",
  "manchester" = "#fdbf6f",
  "chelsea" = "#fdbf6f",
  "arsenal" = "#fdbf6f",
  "liverpool" = "#fdbf6f",
  "city" = "#fdbf6f",
  "deal" = "#fb9a99",
  "transfer" = "#fb9a99",
  "loan" = "#fb9a99",
  "fee" = "#fb9a99",
  "summer" = "#b2df8a",
  "season" = "#b2df8a",
  "striker" = "#a6cee3",
  "club" = "#a6cee3",
  "sky" = "#cab2d6",
  "according" = "#cab2d6",
  "sign" = "#ffff99",
  "move" = "#ffff99",
  "will" = "#ffff99",
  "yearold" = "#d95f02"
)

importance_df$Color <- color_mapping[importance_df$Feature]

ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy, fill = Feature)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = color_mapping) +
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
