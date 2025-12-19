#




library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

df <- read_excel("~/Documents/Post-WIDE/Supplementary Tables 1-7.xlsx", sheet = "Supp T2A. Actionability")

# Define
df <- df %>%
  rename(
    Reimbursed = `Total number of biomarkers guiding reimbursed care`,
    Experimental = `Number of experimental treatment biomarkers`,
  )

df <- df %>%
  mutate(
    Actionability = case_when(
      Reimbursed > 0 & Experimental == 0 ~ "Reimbursed",
      Reimbursed > 0 & Experimental > 0 ~ "Both",
      Reimbursed == 0 & Experimental > 0 ~ "Experimental",
      Reimbursed == 0 & Experimental == 0 ~ "None",
      TRUE ~ NA_character_
    )
  )

df_summary <- df %>%
  count(Tumour_type, Actionability) %>%
  pivot_wider(names_from = Actionability, values_from = n, values_fill = 0)

known_total <- df %>%
  filter(`Diagnosis category` == "Known primary tumour") %>%
  count(Actionability) %>%
  pivot_wider(names_from = Actionability, values_from = n, values_fill = 0) %>%
  mutate(Tumour_type = "All known primary")

blank_row <- tibble(Tumour_type = "Blank", Reimbursed = 0, Both = 0, Experimental = 0, None = 0)

df_combined <- bind_rows(df_summary, known_total, blank_row) %>%
  mutate(
    Tumour_type = ifelse(Tumour_type == "Unknown primary tumour", "All unknown primary", Tumour_type), 
    Total = Reimbursed + Both + Experimental + None)

# Order
visible_tumours <- df_combined %>%
  filter(!(Tumour_type %in% c("Blank", "All known primary", "All unknown primary")) & Total >= 5) %>%
  arrange(desc(Total)) %>%
  pull(Tumour_type)

tumour_levels <- c(visible_tumours, "Blank", "All known primary", "All unknown primary")

df_long <- df_combined %>%
  pivot_longer(cols = c("None", "Experimental", "Both", "Reimbursed"),
               names_to = "Category", values_to = "Count") %>%
  left_join(df_combined %>% select(Tumour_type, Total), by = "Tumour_type") %>%
  mutate(
    Tumour_type = factor(Tumour_type, levels = tumour_levels),
    Category = factor(Category, levels = c("None", "Experimental", "Both", "Reimbursed")), 
    FillKey = case_when(
      Tumour_type == "All unknown primary" & Category == "Reimbursed" ~ "All unknown primary.Reimbursed",
      Tumour_type == "All unknown primary" & Category == "Both" ~ "All unknown primary.Both",
      Tumour_type == "All unknown primary" & Category == "Experimental" ~ "All unknown primary.Experimental",
      TRUE ~ as.character(Category)
    ),
    FillKey = factor(FillKey, levels = c("None", "Experimental", "Both", "Reimbursed", "All unknown primary.Experimental", "All unknown primary.Both", "All unknown primary.Reimbursed")
    )
  ) %>%
  group_by(Tumour_type) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  filter(!is.na(Tumour_type))

# Aesthetics and Plot
ggplot(df_long, aes(x = Tumour_type, y = Percentage, fill = FillKey)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "All unknown primary.Reimbursed" = "#B364D9",
    "All unknown primary.Both" = "#CC96E6",
    "All unknown primary.Experimental" = "#D8BFD8",
    "None" = "grey",
    "Experimental" = "#D3EFFF",
    "Both" = "#A9DCFB",
    "Reimbursed" = "#7FC8F2"
  )) +
  scale_x_discrete(drop = FALSE, limits = levels(df_long$Tumour_type),
                   labels = function(x) ifelse(x == "Blank", "", x)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "",
    x = "",
    y = "Percentage\nof patients",
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(angle = 45, size = 13, family = "Helvetica", hjust = 1), 
    axis.text.y = element_text(size = 12, family = "Helvetica", hjust = 1),
    axis.title.y = element_text(angle = 0, size = 12, vjust = 0.87), 
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = 16),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_line(color = "lightgrey"),  
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 3, 0, 0.5), "cm")
  ) +
  geom_text(
    data = df_combined %>% filter(Tumour_type != "Blank") %>%
      mutate(Tumour_type = factor(Tumour_type, levels = tumour_levels)),
    aes(x = Tumour_type, y = -6, label = paste0("", Total)), 
    inherit.aes = FALSE,
    size = 4.5,
    fontface = "bold",
    family = "Helvetica"
  ) + 
  annotate("text",
           x = length(tumour_levels) + 0.5,  # net rechts van de laatste balk
           y = -5,  # zelfde y-positie als de totalen
           label = "  Number\nof patients",
           hjust = -0.4,
           size = 4,
           family = "Helvetica")



ggsave(
  filename = "output/Figure2D_actionability_cancer_types.pdf",
  width = 12.5,
  height = 6,
  units = "in"
)
