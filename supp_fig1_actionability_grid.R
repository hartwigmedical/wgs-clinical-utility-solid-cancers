# supp_fig1_actionability_grid.R
# Environment:
#   R version 4.4.2
#   RStudio 2024.09.1+394
# Required packages :
#   readxl     (1.4.5) 
#   dplyr      (1.1.4)
#   tidyr      (1.3.1)
#   ggplot2    (3.5.2)
# Input:
#   data/SourceData_Main+ED.xlsx, sheet: SuppFig1
# Output:
#   output/Supp_Fig1_actionability_grid.pdf 

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read_excel("data/SourceData_Main+Ed.xlsx", sheet = "SuppFig1")

req <- c("Tumour_type","Biomarker","Experimental_indications","Reimbursed_indications")
missing <- setdiff(req, names(df))
if (length(missing) > 0) {
  stop(paste("Missing:", paste(missing, collapse = ", ")))
}

data_long <- df %>%
  mutate(
    across(c(Experimental_indications, Reimbursed_indications), as.numeric)
  ) %>%
  pivot_longer(
    cols = c(Experimental_indications, Reimbursed_indications),
    names_to = "Indication_type",
    values_to = "Count"
  ) %>%
  mutate(Indication_type = recode(
    Indication_type,
    "Experimental_indications" = "Experimental treatment",
    "Reimbursed_indications"   = "Reimbursed treatment"
  )) %>%
  complete(Tumour_type, Biomarker, Indication_type, fill = list(Count = 0))

tumour_levels <- sort(unique(data_long$Tumour_type[data_long$Tumour_type != "CUP"]))
data_long <- data_long %>%
  mutate(
    Tumour_type = factor(Tumour_type, levels = c(tumour_levels, "CUP")),
    Biomarker  = factor(Biomarker, levels = rev(sort(unique(Biomarker))))
  )

# Order
data_long <- data_long %>% arrange(Count)

plot_data <- data_long %>%
  filter(Count > 0) %>%
  arrange(Tumour_type, Biomarker, desc(Count), Indication_type)

size_breaks <- pretty(c(0, max(plot_data$Count, na.rm = TRUE)))
size_breaks <- size_breaks[size_breaks > 0]

colors <- c("Reimbursed treatment" = "darkblue",
            "Experimental treatment" = "lightblue")

# Plot
ggplot(plot_data,
       aes(x = Tumour_type, y = Biomarker, size = Count, color = Indication_type)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(2, 10), breaks = size_breaks, name = "Number of Indications") +
  scale_color_manual(values = colors, name = "Indication Type",
                     guide = guide_legend(override.aes = list(size = 6))) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y   = element_text(size = 7),
    plot.title    = element_text(hjust = 0.5, size = 8, face = "bold"),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 8),
    legend.key.size = unit(1.2, "cm"),
    axis.title.x  = element_text(size = 10)
  ) + 
  coord_cartesian(clip = "off") +                          
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  theme(
    plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5)
  )

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  "output/supp_fig1_actionability_grid.pdf", 
  width = 8, 
  height = 10, 
  units = "in")
