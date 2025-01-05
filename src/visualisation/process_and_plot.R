final_df <- final_df_with_gpt_3.5
# final_df <- final_df_with_gpt_3.5 %>% filter(fact.real_rumor == T)

clean_currency = function(x) {
  x <- gsub("\\€", "", x)
  
  multiplier <- ifelse(grepl("k$", x), 1000,
                       ifelse(grepl("m$", x), 1000000, 1))
  x <- as.numeric(substring(x, 1, nchar(x)-1)) * multiplier
  
  return(x)
}

final_df <- final_df %>%
  mutate(
    Rumor.Source = sub("https://www.?", "https://", Rumor.Source),
    Rumor.Source = sub("https://([a-zA-Z0-9-]+)\\.[a-z]+", "\\1", Rumor.Source),
    Rumor.Source = sub("([a-zA-Z0-9-]+)\\.[a-z]+", "\\1", Rumor.Source),
    fact.marketValue = clean_currency(fact.marketValue),
    fact.fee = clean_currency(fact.fee)
  ) %>% filter(!is.na(fact.fee))

final_df$GPT.confidence = as.factor(final_df$GPT.confidence)
final_df$GPT.language = tolower(final_df$GPT.language)

final_df <- data.frame(final_df, 
                       accuracy = ifelse(final_df$fact.real_rumor, 
                                         as.numeric(final_df$GPT.confidence), 
                                         -as.numeric(final_df$GPT.confidence)))



#### 0.Yearly Dynamics
ggplot(data = final_df, aes(x = Year, y = accuracy)) +
  geom_smooth(se = TRUE, color = "seagreen") +
  theme_minimal() +
  labs(x = "Year", y = "Accuracy", title = "Accuracy Trends Across Years") 

ggplot(data = final_df, aes(x = Year)) +
  geom_density(fill = "#F6D860", color = NA, alpha = 0.8) +  # Soft yellowish color
  theme_minimal() +
  labs(x = "Year", y = "Density", title = "Yearly Rumour Amount Density")



#### 1.Dynamics within Year
final_df$Year <- as.numeric(substr(final_df$Rumor.FormatDate, 1, 4))
final_df$Month <- as.numeric(substr(final_df$Rumor.FormatDate, 5, 6))
final_df$Day <- as.numeric(substr(final_df$Rumor.FormatDate, 7, 8))
final_df$Hour <- as.numeric(substr(final_df$Rumor.FormatDate, 9, 10))

# Create a continuous time variable within each year
final_df$YearFraction <- final_df$Month/12 + 
  final_df$Day/365 + 
  final_df$Hour/8760

final_df_wrapped <- bind_rows(
  final_df %>% mutate(YearFraction = YearFraction - 1),  # Prepend data from end of previous year
  final_df,
  final_df %>% mutate(YearFraction = YearFraction + 1)  # Append data to start of next year
)

ggplot(data = final_df_wrapped, aes(x = YearFraction, y = accuracy)) + 
  geom_smooth(se = TRUE, color = "seagreen") +  
  labs(x = "Time within Year", y = "Accuracy",
       title = "Temporal Accuracy Dynamics by Year") +
  xlim(0, 1) +  # Constrain x-axis to [0,1]
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  
  # Winter transfer window highlight (Jan 1 to Feb 3)
  annotate("rect", 
           xmin = 0, xmax = (33/365), 
           ymin = -Inf, ymax = Inf, 
           fill = "lightblue", alpha = 0.3) +
  annotate("text", 
           x = (33/365)/2, 
           y = Inf, 
           label = "Winter\nTransfer\nWindow", 
           vjust = 6.5, 
           size = 4,  # Increased size
           fontface = "bold",  # Made bold
           color = "darkblue") + 
  
  # Summer transfer window highlight (Jun 14 to Aug 30)
  annotate("rect", 
           xmin = (165/365), xmax = (242/365), 
           ymin = -Inf, ymax = Inf, 
           fill = "lightsalmon", alpha = 0.3) +
  annotate("text", 
           x = mean(c(165/365, 242/365)), 
           y = Inf, 
           label = "Summer\nTransfer\nWindow", 
           vjust = 6.5, 
           size = 4,  # Increased size
           fontface = "bold",  # Made bold
           color = "darkred") +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

accuracy_by_language = final_df %>%
  group_by(GPT.language) %>%
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE))




#### 2.Plot by Language
top_languages <- final_df %>%
  count(GPT.language) %>%
  arrange(desc(n)) %>%
  slice(1:8) %>%
  pull(GPT.language)

dat_by_language <- final_df %>%
  mutate(
    GPT.language = ifelse(GPT.language %in% top_languages, GPT.language, "Others"),
    GPT.language = str_to_title(GPT.language)  # Capitalize first letter of each label
  ) %>%
  count(GPT.language) %>%  # Count occurrences of each group
  mutate(
    GPT.language = fct_reorder(GPT.language, -n),  # Rank groups by count (descending)
    GPT.language = fct_relevel(GPT.language, "Others", after = Inf)  # Move "Others" to the end
  )

ggplot(data = dat_by_language, aes(x = GPT.language, y = n)) +
  geom_col(fill = "#F6D860", alpha = 0.8) +  # Soft yellowish color
  theme_minimal() +
  labs(x = "Language", y = "Count", title = "Rumour Amount Across Languages") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
