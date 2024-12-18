source('__init__.R')
source('utils.R')

df <- final_df_with_gpt_3.5
df <- filter(df, !is.na(df$fact.url))
# df <- filter(df, df$Rumor.Language == "english")
df <- downSample(df)
df$fact.real_rumor <- factor(df$fact.real_rumor, levels = c(FALSE, TRUE))
df$Rumor.Source <- sapply(df$Rumor.Source, cleanSource)

X_content <- textPrep(df$Rumor.Content, 0.99)
X_Source <- textPrep(df$Rumor.Source, 0.99)

df$Month <- substr(df$Rumor.FormatDate, 5, 6)
df$Month <- as.numeric(df$Month)

# randomForest
X <- cbind(X_content, X_Source, df$GPT.confidence, df$Month, df$Nationality, df$Position)
y <- df$fact.real_rumor
model <- randomForestPipeline(X, y, p = 0.7, ntree = 300)

importance_df <- as.data.frame(importance(model)) %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(30)


ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  theme_minimal() +
  labs(
    title = "Variable Importance",
    x = "Feature",
    y = "Mean Decrease Gini"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none"
  )

# svm
X <- cbind(X_content, X_Source, df$GPT.confidence, df$Month, model.matrix(~ df$Nationality - 1), model.matrix(~ df$Position - 1))
y <- df$fact.real_rumor
model <- svmPipeline(X, y, p = 0.7, kernel = 'radial', cost = 3)

# elasticNet
X <- cbind(X_content, X_Source, df$GPT.confidence, df$Month, model.matrix(~ df$Nationality - 1))
y <- as.numeric(df$fact.real_rumor) - 1
model <- elasticNetPipeline(X, y, p = 0.7, alpha = 0.5)
