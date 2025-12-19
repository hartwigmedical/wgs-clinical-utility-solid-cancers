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
#   (Final layout refinements and labels were adjusted in Inkscape)

library(ggplot2)
library(dplyr)
library(readxl)

# Input
infile <- "~/Documents/Post-WIDE/SourceData_Main+ED.xlsx"
sheet  <- "Fig2C"

df <- read_excel(infile, sheet = sheet) %>%
  mutate(
    # robust EU decimal handling: "3,2" -> 3.2
    TMB = suppressWarnings(as.numeric(gsub(",", ".", `TMB (in variants/Mb)`))),
    `Diagnosis category` = factor(
      `Diagnosis category`,
      levels = c("Known primary tumour", "Cancer of unknown primary")
    )
  ) %>%
  filter(!is.na(TMB), !is.na(`Diagnosis category`))

df_known <- df %>% filter(`Diagnosis category` == "Known primary tumour")
df_cup   <- df %>% filter(`Diagnosis category` == "Cancer of unknown primary")

# Statistics: median
median_known <- median(df_known$TMB, na.rm = TRUE)
median_cup   <- median(df_cup$TMB,   na.rm = TRUE)

d_known <- density(df_known$TMB, na.rm = TRUE)
d_cup   <- density(df_cup$TMB,   na.rm = TRUE)

peak_known <- max(d_known$y)
peak_cup   <- max(d_cup$y)

yend_known <- 0.95 * peak_known
yend_cup   <- -0.95 * peak_cup

ytext_known <- 0.45 * peak_known
ytext_cup   <- -0.45 * peak_cup

# Aesthetics
p <- ggplot() +
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
  geom_segment(aes(x = median_known, xend = median_known, y = 0, yend = yend_known),
               colour = "black", linewidth = 1) +
  geom_segment(aes(x = median_cup,   xend = median_cup,   y = 0, yend = yend_cup),
               colour = "black", linewidth = 1) +
  annotate("text", x = median_known, y = ytext_known,
           label = sprintf("%.1f", median_known),
           size = 5, fontface = "bold", vjust = -0.5) +
  annotate("text", x = median_cup, y = ytext_cup,
           label = sprintf("%.1f", median_cup),
           size = 5, fontface = "bold", vjust = 1.2) +
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

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  filename = "output/Figure2C_TMB_density.pdf",
  width = 4.0,
  height = 4.0,
  units = "in"
)
