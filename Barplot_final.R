# Load libraries
library(tibble)
library(ggplot2)
library(patchwork)

# Common Theme
base_text_size <- 18
common_theme <- theme_classic(base_size = base_text_size) +
  theme(
    plot.title = element_text(size = base_text_size + 2, face = "bold"),
    axis.text.y = element_text(size = base_text_size),
    axis.title.x = element_text(size = base_text_size)
  )

# (a) NbS Type 
nbs <- c(
  "Ecosystem restoration", "Green buildings", "Urban spaces", "Water management",
  "Agriculture", "Forestry", "Tourism & wellbeing", "Advisory", "Education & research",
  "Finance", "Smart tech", "Other"
)

nbs_count <- c(207, 6, 23, 35, 24, 21, 11, 24, 60, 12, 19, 1)

nbs_df <- tibble(NbS = nbs, Count = nbs_count)

plot_a <- ggplot(nbs_df, aes(x = reorder(NbS, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkolivegreen") +
  coord_flip() +
  labs(title = "(a) Types of NbS", x = NULL, y = "Number of studies") +
  common_theme

# (b) SDGs 
sdg_labels <- c(
  "No poverty", "Zero hunger", "Health", "Education", "Gender equality", "Water & sanitation",
  "Clean energy", "Decent work", "Innovation", "Reduced inequalities", "Sustainable cities",
  "Consumption", "Climate action", "Life below water", "Life on land", "Peace & justice", "Partnerships"
)
sdg_counts <- c(16, 23, 33, 15, 11, 49, 8, 29, 8, 15, 69, 23, 133, 109, 160, 3, 53)

sdg_df <- tibble(SDG = sdg_labels, Count = sdg_counts)

plot_b <- ggplot(sdg_df, aes(x = reorder(SDG, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkolivegreen") +
  coord_flip() +
  labs(title = "(b) SDG Links", x = NULL, y = "Number of studies") +
  common_theme

# (c) Societal Challenges 
societal_challenges <- c(
  "Low provisioning", "Flooding", "Extreme events", "Soil fertility", "Erosion", "Air quality",
  "Water quality", "Carbon storage", "Drought & heat", "Biodiversity loss", "Invasive species",
  "Aesthetics", "Tourism", "Unemployment", "Lack of ownership", "Social inclusion",
  "Investment in NbS", "Health impacts", "Knowledge gaps", "None", "Other"
)

societal_counts <- c(
  53, 47, 54, 33, 64, 9, 41, 54, 70, 175, 37, 10, 22, 45, 22, 28, 60, 17, 82, 2, 0
)

societal_df <- tibble(Challenge = societal_challenges, Count = societal_counts)

plot_c <- ggplot(societal_df, aes(x = reorder(Challenge, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkolivegreen") +
  coord_flip() +
  labs(title = "(c) Societal Challenges", x = NULL, y = "Number of studies") +
  common_theme

# (d) Co-benefits
co_benefits <- c(
  "Provisioning", "Flood protection", "Extreme events", "Soil fertility", "Erosion control",
  "Air quality", "Water quality", "Carbon storage", "Cooling & drought", "Biodiversity",
  "Invasive species", "Aesthetics", "Tourism", "Green jobs", "Ownership", "Social inclusion",
  "Investment", "Health", "Education", "None", "Other"
)

co_counts <- c(
  73, 47, 50, 38, 67, 10, 55, 50, 60, 200, 38, 17, 28, 53, 37, 46, 60, 28, 115, 2, 0
)

cobenefit_df <- tibble(CoBenefit = co_benefits, Count = co_counts)

plot_d <- ggplot(cobenefit_df, aes(x = reorder(CoBenefit, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkolivegreen") +
  coord_flip() +
  labs(title = "(d) Co-benefits", x = NULL, y = "Number of studies") +
  common_theme

#  Combine All with Patchwork
combined_plot3 <- (plot_a | plot_b) / (plot_c | plot_d)

# Save Final Figure
#ggsave(
 # filename = "nbs_4plots_2x2_readable.png",
  #plot = combined_plot3,
  #dpi = 300,
  #width = 22,   # wider to fit long y-axis labels
  #height = 18,  # enough vertical space for two rows
  #units = "in"
#)
