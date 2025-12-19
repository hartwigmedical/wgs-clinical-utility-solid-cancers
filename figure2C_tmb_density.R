# figure2C_tmb_density.R
# Purpose: Generate the density plot underlying Figure 2C (TMB distribution by diagnosis category)
# Environment:
#   R version 4.4.2
#   RStudio 2024.09.1+394
# Required packages:
#   readxl  (v1.4.5)
#   ggplot2 (v3.5.2)
#   dplyr   (v1.1.4)
# Input:
#   data/SourceData_Main+ED.xlsx
#   Sheet: "Fig2C"
# Output:
#   output/Figure2C_tmb_density.pdf
#   (Final layout refinements and label positioning were adjusted in Inkscape)

library(ggplot2)
library(dplyr)
library(readxl)

# Input
df <- read_excel(
  path  = "data/SourceData_Main+ED.xlsx",
  sheet = "Fig2C"
) %>%
  mutate(
    TMB = suppressWarnings(as.numeric(gsub(",", ".", `TMB (in variants/Mb)`))),
    `Diagnosis category` = factor(
      `Diagnosis category`,
      levels = c("Known primary tumour", "Cancer of unknown primary")
    )
  ) %>%
  filter(!is.na(TMB), !is.na(`Diagnosis category`)) %>%
  filter(TMB >= 0.1, TMB <= 1000)

df_known <- df %>% filter(`Diagnosis category` == "Known primary tumour")
df_cup   <- df %>% filter(`Diagnosis category` == "Cancer of unknown primary")

# Statistics: median
median_known <- median(df_known$TMB, na.rm = TRUE)
median_cup   <- median(df_cup$TMB,   na.rm = TRUE)

# Aesthetics
p_base <- ggplot() +
  geom_density(
    data = df_known,
    aes(x = TMB, y = after_stat(density)),
    fill = "#A9DCFB", alpha = 0.7, colour = NA
  ) +
  geom_density(
    data = df_cup,
    aes(x = TMB, y = after_stat(-density)),
    fill = "#CC96E6", alpha = 0.7, colour = NA
  ) +
  scale_x_log10(
    limits = c(0.1, 1000),
    breaks = c(0.1, 1, 10, 100, 1000),
    labels = c("0.1", "1", "10", "100", "1000")
  ) +
  labs(x = "mut/Mb", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y        = element_blank(),
    axis.title.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position    = "none"
  )

b <- ggplot_build(p_base)
peak_known <- max(b$data[[1]]$y, na.rm = TRUE)
peak_cup   <- abs(min(b$data[[2]]$y, na.rm = TRUE)) 

label_shift <- 0.90

round_half_up <- function(x, digits = 1) {
  f <- 10^digits
  ifelse(x >= 0,
         floor((x + 1e-12) * f + 0.5) / f,
         ceiling((x - 1e-12) * f - 0.5) / f)
}

lab_known <- round_half_up(median_known, 1)
lab_cup   <- round_half_up(median_cup, 1)

# Combine
p_final <- p_base +
  geom_segment(aes(x = median_known, xend = median_known, y = 0, yend = peak_known),
               colour = "black", linewidth = 1) +
  geom_segment(aes(x = median_cup,   xend = median_cup,   y = 0, yend = -peak_cup),
               colour = "black", linewidth = 1) +
  annotate("text", x = median_known * label_shift, y = 0.45 * peak_known,
           label = sprintf("%.1f", lab_known), fontface = "bold", size = 5, hjust = 1) +
  annotate("text", x = median_cup * label_shift, y = -0.45 * peak_cup,
           label = sprintf("%.1f", lab_cup), fontface = "bold", size = 5, hjust = 1)


p_final


# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  "output/Figure2C_tmb_density.pdf", 
  width = 4.0, 
  height = 4.0, 
  units = "in"
)
