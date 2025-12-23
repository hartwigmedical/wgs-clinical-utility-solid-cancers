# figure2B_actionability_counts.R
# Purpose: Generate Figure 2B: number of patients by number of actionable biomarkers (0, 1, 2, 3+), stratified by diagnosis category (known primary tumour vs CUP)
# Environment:
#   R version 4.4.2
#   RStudio 2024.09.1+394
# Required packages:
#   readxl  (v1.4.5)
#   dplyr   (v1.1.4)
#   ggplot2 (v3.5.2)
#   readr   (v2.1.5)
# Input:
#   data/SourceData_Main+ED.xlsx, sheet: Fig2B
# Output:
#   output/Figure2B_actionability_counts.pdf (Final layout refinements were made in Inkscape before submission)

library(readxl)
library(dplyr)
library(ggplot2)
library(readr)

# Input
df <- read_excel(
  path  = "data/SourceData_Main+ED.xlsx",
  sheet = "Fig2B"
)

# Define
df <- df %>%
  mutate(
    actionable_num = parse_number(as.character(`Actionable only`)),
    BiomarkerGroup = case_when(
      actionable_num == 0 ~ "0",
      actionable_num == 1 ~ "1",
      actionable_num == 2 ~ "2",
      actionable_num >= 3 ~ "3+",
      TRUE ~ NA_character_
    ),
    `Diagnosis category` = factor(
      `Diagnosis category`,
      levels = c("Known primary tumour", "Cancer of unknown primary")
    )
  ) %>%
  filter(!is.na(BiomarkerGroup), !is.na(`Diagnosis category`))

plot_data <- dfd %>%
  count(BiomarkerGroup, `Diagnosis category`, name = "n")

# Aesthetics
colors <- c("Known primary tumour" = "#a5dbfaff",
            "Cancer of unknown primary" = "#c992e6ff")

p <- ggplot(plot_data, aes(x = BiomarkerGroup, y = n, fill = `Diagnosis category`)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.55) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.7),
            vjust = -0.5, fontface = "bold", size = 5, family = "Helvetica") +
  scale_fill_manual(values = colors) +
  labs(x = "Number     \nof actionable\nbiomarkers ",
       y = "Number  \nof patients") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13, family = "Helvetica"),
    axis.title.x = element_text(hjust = 1.3, vjust = 10, size = 12, family = "Helvetica"),
    axis.title.y = element_text(angle = 0, hjust = 0.95, vjust = 0.85, size = 12, family = "Helvetica"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  "output/Figure2B_actionability_counts.pdf", 
   width = 6.5, 
   height = 5.5, 
   units = "in"
)
