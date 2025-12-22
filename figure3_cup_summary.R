# figure3_cup_summary.R
# Purpose: Reproduces Figure 3 (CUP outcome categories + treatment categories), without hard-coded counts.
# Environment:
#   R version 4.4.2
#   RStudio 2024.09.1+394
# Required packages:
#   readxl  (1.4.5)
#   dplyr   (1.1.4)
#   tidyr   (1.3.1)
#   stringr (1.5.1)
#   ggplot2 (3.5.2)
#   cowplot (1.1.3)
#   scales  (1.3.0)
# Input:
#   data/SourceData_Main+ED.xlsx, sheet "Fig3"
# Output:
#   output/figure3_cup_summary.pdf
# Notes:
#   Layout (panel placement) mirrors the original script; only the data are computed from Source Data.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(scales)

# Input
df <- read_excel(
  "data/SourceData_Main+ED.xlsx", 
  sheet = "Fig3") 
  

df <- read_excel(file, sheet = sheet) %>%
  mutate(
    CUP_solution         = str_squish(as.character(CUP_solution)),
    Post_CUP_diagnosis   = str_squish(as.character(Post_CUP_diagnosis)),
    WGS_informed_treatment = str_squish(as.character(WGS_informed_treatment)),
    Followup_CUP         = toupper(str_squish(as.character(Followup_CUP)))
  )

# Define
df <- df %>%
  mutate(
    CUP_solution_std = str_remove(CUP_solution, "\\.+$"),
    solution_group = case_when(
      CUP_solution_std == "Case not fully solved by WGS" ~ "Not completely solved",
      CUP_solution_std == "Diagnosis indicated with lower certainty by WGS" ~ "Aided CUP solution",
      CUP_solution_std == "Diagnosis given with high certainty" ~ "CUP definitively solved",
      TRUE ~ NA_character_
    ),
    solution_group = factor(
      solution_group,
      levels = c("Not completely solved", "Aided CUP solution", "CUP definitively solved")
    )
  ) %>%
  filter(!is.na(solution_group))

treat_levels <- c("No treatment data", "No systemic treatment", "CUP regimen", "Non-targeted",
  "Standard care", "Exp. trial", "Reimbursed")

df <- df %>%
  mutate(
    treatment_cat = case_when(
      # Exacte categorieën (zoals jij aangeeft dat ze in de sheet staan)
      WGS_informed_treatment == "Biomarker-informed reimbursed treatment" ~ "Reimbursed",
      WGS_informed_treatment == "Biomarker-informed experimental treatment" ~ "Exp. trial",
      WGS_informed_treatment == "Non-biomarker-informed standard-of-care treatment" ~ "Standard care",
      WGS_informed_treatment == "Non-biomarker-informed experimental treatment" ~ "Non-targeted",
      WGS_informed_treatment == "CUP regimen" ~ "CUP regimen",
      WGS_informed_treatment == "No treatment" & Followup_CUP == "YES" ~ "No systemic treatment",
      WGS_informed_treatment == "No treatment" & Followup_CUP != "YES" ~ "No treatment data",
      TRUE ~ NA_character_
    ),
    treatment_cat = factor(treatment_cat, levels = treat_levels)
  )

pie_data <- df %>%
  count(solution_group, name = "Count") %>%
  mutate(
    Percent = percent(Count / sum(Count), accuracy = 1),
    Category = as.character(solution_group),
    Color = case_when(
      Category == "Not completely solved" ~ "grey",
      Category == "Aided CUP solution" ~ "#CC96E6",
      Category == "CUP definitively solved" ~ "#B364D9",
      TRUE ~ "grey"
    )
  ) %>%
  select(Category, Count, Percent, Color)

pie_data$Category <- factor(pie_data$Category, levels = solution_levels)

pie_plot <- ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pie_data$Color) +
  geom_text(aes(label = paste0(Count, "\n", Percent)),
            position = position_stack(vjust = 0.5),
            color = "white", family = "Helvetica", fontface = "bold", size = 5) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -14),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, family = "Helvetica"),
    legend.key.size = unit(0.9, "lines"),
    legend.key.spacing.y = unit(0.3, "lines")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

bar_counts <- df %>%
  count(solution_group, treatment_cat, name = "Freq") %>%
  complete(solution_group, treatment_cat, fill = list(Freq = 0))

make_bar_plot <- function(sol_group, show_axis_labels = TRUE) {
  dd <- bar_counts %>%
    filter(solution_group == sol_group) %>%
    mutate(outcomes = factor(as.character(treatment_cat), levels = rev(treat_levels)))
  
  ggplot(dd, aes(x = "", y = Freq, fill = outcomes)) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_fill_manual(values = c(
      "No treatment data" = "#999999ff",
      "No systemic treatment" = "lightgrey",
      "Non-targeted" = "#ccf3c5ff",
      "Standard care" = "#7ed957ff",
      "Exp. trial" = "#cce6ffff",
      "Reimbursed" = "#7fc8f2ff",
      "CUP regimen" = "#c39bd3ff"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = if (show_axis_labels) element_text(size = 9, family = "Helvetica") else element_blank(),
      axis.title = element_blank(),
      axis.line.y = element_line(color = "lightgrey"),
      axis.ticks.y = element_line(color = "lightgrey"),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

bar_plot_unsolved <- make_bar_plot("Not completely solved", show_axis_labels = TRUE)
bar_plot_aided    <- make_bar_plot("Aided CUP solution", show_axis_labels = TRUE)
bar_plot_solved   <- make_bar_plot("CUP definitively solved", show_axis_labels = TRUE)

# Legend
legend_bar <- get_legend(
  ggplot(bar_counts %>% filter(solution_group == "Not completely solved") %>%
           mutate(outcomes = factor(as.character(treatment_cat), levels = rev(treat_levels))),
         aes(x = "", y = Freq, fill = outcomes)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
      values = c(
        "No treatment data" = "#999999ff",
        "No systemic treatment" = "lightgrey",
        "Non-targeted" = "#ccf3c5ff",
        "Standard care" = "#7ed957ff",
        "Exp. trial" = "#cce6ffff",
        "Reimbursed" = "#7fc8f2ff",
        "CUP regimen" = "#c39bd3ff"
      ),
      labels = c(
        "No treatment data" = "No treatment data",
        "No systemic treatment" = "No systemic treatment",
        "Non-targeted" = "Non-biomarker-informed experimental treatment",
        "Standard care" = "Non-biomarker-informed standard-of-care treatment",
        "Exp. trial" = "Biomarker-informed experimental treatment",
        "Reimbursed" = "Biomarker-informed reimbursed treatment",
        "CUP regimen" = "CUP regimen"
      )
    ) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 9.5, family = "Helvetica"),
      legend.key.size = unit(0.9, "lines"),
      legend.key.spacing.y = unit(0.3, "lines")
    )
)

# Combine
pie_with_frame <- ggdraw() +
  theme(plot.background = element_rect(color = "grey90", fill = NA, linewidth = 1.5)) +
  draw_line(x = c(0.1, -0.05), y = c(0.76, 0.82), color = "lightgrey", linetype = "dashed", size = 0.7) +
  draw_line(x = c(0.95, 1.07), y = c(0.61, 0.61), color = "lightgrey", linetype = "dashed", size = 0.7) +
  draw_line(x = c(0.35, -0.05), y = c(0.335, 0.23), color = "lightgrey", linetype = "dashed", size = 0.7) +
  draw_plot(pie_plot, x = -0.05, y = 0.00, width = 1.1, height = 1.1) 

final_plot <- ggdraw() +
  draw_plot(pie_with_frame,     x = 0.2,   y = 0.21,  width = 0.4,  height = 0.59) +
  draw_plot(bar_plot_unsolved,  x = 0.098, y = 0.52,  width = 0.09, height = 0.3) +
  draw_plot(bar_plot_aided,     x = 0.098, y = 0.185, width = 0.09, height = 0.3) +
  draw_plot(bar_plot_solved,    x = 0.605, y = 0.52,  width = 0.09, height = 0.3) +
  draw_plot(legend_bar,         x = 0.784,   y = 0.191, width = 0.2,  height = 0.3) +
  theme(plot.margin = margin(t=0, r=1, b=0, l=0, unit = "in"))

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  filename = "output/figure3_cup_summary.pdf",
  final_plot,
  width = 7.50,
  height = 6.60,
  units = "in"
)
