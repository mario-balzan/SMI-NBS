#trying LDA with Gibbs Sampling 
#dtm already with word cleaning?

library(readxl)
library(tidyverse)
library(tm)
library(topicmodels)
library(textstem)
library(textclean)
library(dplyr)
library(ggplot2)

# Load the updated Excel file
data <- read_excel("Summary statistics1.xlsx", sheet = "Final dataset")


# Ensure the title and description columns exist
text_data <- data %>%
  mutate(
    title = as.character(`Title of project or paper`),
    description = as.character(`Short description of the project`)
  ) %>%
  unite("combined_text", c(title, description), sep = " ", na.rm = TRUE) %>%
  select(combined_text)

text_data

custom_stopwords <- c(
  stopwords("en"),
  "project", "area", "island", "use", "will", "study", "local", "ref",
  "les", "des", "ont", "pour", "dans", "une", "site", "garden", "datum", "los", "para", 
  "increase", "improve", "locate", "use", "also", "involve", "need", "site", "low", "significant", 
  "focus", "year", "new", "use", "provide", "include", "within", "aim", "good", "las", "high", "can",
  "program", "que", "two", "one", "first", "good", "include", 
)

clean_text <- text_data$combined_text %>%
  replace_non_ascii() %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  lemmatize_strings() %>%         # lemmatise first
  removeWords(custom_stopwords)  # then remove stopwords

# Create corpus and document-term matrix
corpus <- Corpus(VectorSource(clean_text))
dtm <- DocumentTermMatrix(corpus,
                          control = list(
                            wordLengths = c(3, Inf),
                            bounds = list(global = c(2, Inf))
                          ))

# Remove empty documents
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# Run LDA using Gibbs sampling
lda_gibbs <- LDA(
  dtm,
  k = 10,
  method = "Gibbs",
  control = list(
    seed = 42,       # reproducibility
    burnin = 1000,   # number of burn-in iterations to discard
    iter = 2000,     # total number of Gibbs sampling iterations
    thin = 100       # thinning interval
  )
)

# Extract and display top terms for each topic
top_termsG <- terms(lda_gibbs, 10)
for (i in 1:nrow(top_termsG)) {
  cat(paste0("Topic ", i, ": "), paste(top_termsG[i, ], collapse = ", "), "\n")
}


# Extract top 10 terms for each topic
top_termsG <- terms(lda_gibbs, 10)


####

# Get the term-topic matrix (beta values)
term_matrix <- as.matrix(lda_gibbs@beta)

# Get vocabulary (terms)
vocab <- lda_gibbs@terms

# Convert to tidy format
# Extract topic-term probabilities (beta matrix)
beta_matrix <- posterior(lda_gibbs)$terms  # [topic x term] matrix

# Get vocabulary (terms)
vocab <- colnames(beta_matrix)

# Get top terms for each topic with their probabilities
top_terms_df <- map_dfr(1:nrow(beta_matrix), function(topic) {
  term_probs <- beta_matrix[topic, ]
  top_indices <- order(term_probs, decreasing = TRUE)[1:10]
  tibble(
    Topic = paste("Topic", topic),
    Term = vocab[top_indices],
    Probability = term_probs[top_indices]
  )
})

ggplot(top_terms_df, aes(x = reorder(Term, Probability), y = Probability, fill = Topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic, scales = "free") +
  coord_flip() +
  labs(
    title = "Top Terms per Topic with Probabilities",
    x = "Term",
    y = "Probability"
  ) +
  theme_minimal()

ggplot(top_terms_df, aes(x = reorder_within(Term, Probability, Topic), 
                         y = Probability, fill = Topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +  # Required to make reorder_within() work
  labs(
    title = "Top Terms per Topic (Ordered by Probability)",
    x = "Term",
    y = "Probability"
  ) +
  theme_minimal()

top_terms_df <- top_terms_df %>%
  mutate(Topic = factor(Topic, levels = paste("Topic", 1:10)))


topic_key <- data.frame(
  Topic = paste("Topic", 1:10),
  Title = c(
    "Ecosystem services",
    "Marine ecosystems",
    "Soil management",
    "Biodiversity conservation",
    "Endangered insular species",
    "Community-based climate adaptation",
    "Invasive species control",
    "Coral reef conservation",
    "Coastal ecosystems and resilience",
    "Urban green and blue infrastructure"
  )
)



library(tidytext)
ggplot(top_terms_df, aes(x = reorder_within(Term, Probability, Topic), 
                         y = Probability, fill = Topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms per Topic (Ordered by Probability)",
    x = "Term",
    y = "Probability"
  ) +
  theme_minimal() 


#ggsave(
 # filename = "10topics_terms_gibbs.png",  
  #dpi = 300,                            
  #width = 12,                           
  #height = 8,                           
  #units = "in"
#)

####


##### Assign dominant topic to each document
topic_probabilities <- posterior(lda_gibbs)$topics
dominant_topic <- apply(topic_probabilities, 1, which.max)

# Match dominant topics back to original dataset (accounting for removed empty rows)
data_filtered <- data[rowSums(as.matrix(dtm)) > 0, ]
data_filtered$Dominant_Topic <- dominant_topic

# Summarise distribution of dominant topics by FAO Major Fishing Area
topic_distribution <- data_filtered %>%
  group_by(`FAO Major Fishing Area`, Dominant_Topic) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Dominant_Topic, values_from = Count, values_fill = 0)

# View the result
print(topic_distribution)

# Heatmap using ggplot2
topic_distribution_long <- data_filtered %>%
  group_by(`FAO Major Fishing Area`, Dominant_Topic) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(topic_distribution_long, aes(x = factor(Dominant_Topic), y = factor(`FAO Major Fishing Area`))) +
  geom_tile(aes(fill = Count)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Topic", y = "FAO Fishing Area", fill = "Count") +
  theme_minimal()

#dealing with multiple fishing zones
data_filtered <- data_filtered %>%
  mutate(
    FAO_Clean = ifelse(str_detect(`FAO Major Fishing Area`, ","), 
                       "Multiple zones", 
                       as.character(`FAO Major Fishing Area`))
  )

# Define topic labels
topic_labels <- c(
  "Ecosystem services", "Marine ecosystem", "Soil management",
  "Biodiversity conservation", "Endangered insular species", "Community climate adaptation",
  "Invasive Species Control", "Coral reef conservation", "Coastal resilience", "Urban green and blue\ninfrastructure"
)

# Define FAO area names
fao_lookup <- data.frame(
  FAO_Clean = c("2", "21", "27", "31", "34", "37", "41", "47", "51", "57", 
                "61", "67", "71", "77", "81", "87", "88"),
  FAO_Name = c(
    "N America Inland Waters", "NW Atlantic", "NE Atlantic", "W Central Atlantic", "E Central Atlantic",
    "Mediterranean & Black Sea", "SW Atlantic", "SE Atlantic", "W Indian Ocean", "E Indian Ocean",
    "NW Pacific", "NE Pacific", "W Central Pacific", "E Central Pacific", "SW Pacific",
    "SE Pacific", "Antarctic Pacific"
  )
)

# Summarise the counts of dominant topics by FAO zone
topic_distribution_long <- data_filtered %>%
  group_by(FAO_Clean, Dominant_Topic) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Topic_Label = factor(Dominant_Topic, levels = 1:10, labels = topic_labels)
  ) %>%
  left_join(fao_lookup, by = "FAO_Clean") %>%
  mutate(
    FAO_Label = ifelse(is.na(FAO_Name), "Multiple Zones", paste0(FAO_Clean, " - ", FAO_Name))
  )

# Plot heatmap
ggplot(topic_distribution_long, aes(x = Topic_Label, y = reorder(FAO_Label, desc(FAO_Label)))) +
  geom_tile(aes(fill = Count)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    x = "Topic",
    y = "FAO Fishing Area",
    fill = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####
#ggsave(
 # filename = "FAO_heatmap_gibbs_titles1.png",  # change to .pdf or .tiff if needed
  #dpi = 300,                            # high resolution
  #width = 12,                           # width in inches
  #height = 8,                           # height in inches
  #units = "in"
#)



