source('__init__.R')
source('utils.R')

df <- final_df_with_gpt_3.5
df <- filter(df, !is.na(df$fact.url))
df <- downSample(df)
df$y <- as.factor(df$fact.real_rumor)
df$Rumor.Source <- sapply(df$Rumor.Source, cleanSource)
X_source <- textPrep(df$Rumor.Source, 0.995)

model <- randomForest(X_source, df$y, ntree = 200, importance = TRUE)
importance_df <- as.data.frame(importance(model)) %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, MeanDecreaseAccuracy = MeanDecreaseAccuracy) %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  head(20)


palette <- rep(brewer.pal(8, "Set2"), length.out = nrow(importance_df))
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy, fill = Feature)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = palette) +
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
