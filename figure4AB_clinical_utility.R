# figure4AB_clinical_utility.R
# Purpose: Tile plots for clinical utility parameters per sample (Known vs CUP)
# Environment:
#   R 4.4.2; RStudio 2024.09.1+394
# Packages: 
#   readxl  (1.4.5)
#   dplyr   (1.1.4)
#   tidyr   (1.3.1)
#   ggplot2 (3.5.2)
# Input: 

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Input
data_path <- file.path("data/SourceData_Main+ED.xlsx")
sheet_name <- "Fig4"

df <- read_excel(
  "data/SourceData_Main+ED.xlsx", 
  sheet = "Fig4") %>%
  mutate(
    Reimbursed          = as.integer(Reimbursed),
    CUP_algorithm       = as.integer(CUP_algorithm),
    Clinically_relevant = as.integer(Clinically_relevant),
    Germline            = suppressWarnings(as.integer(Germline)),
    Has_Germline        = !is.na(Germline),
    Germline_bin        = ifelse(Has_Germline, Germline, 0L)
  )

add_priority <- function(df_sub, order_vec) {
  key <- paste0(df_sub$Reimbursed, df_sub$Germline_bin, df_sub$CUP_algorithm)
  df_sub$Priority <- match(key, order_vec)
  df_sub$Priority[is.na(df_sub$Priority)] <- length(order_vec) + 1L
  df_sub
}

make_tile_panel <- function(df_all, diagnosis_label, order_vec, parameter_order, palette) {
  df_sub <- df_all %>%
    filter(Diagnosis == diagnosis_label) %>%
    add_priority(order_vec = order_vec) %>%
    arrange(Priority, desc(Has_Germline)) %>%
    mutate(Number = factor(Number, levels = unique(Number)))
  
  df_long <- df_sub %>%
    select(Number, Reimbursed, Germline_bin, CUP_algorithm, Clinically_relevant, Has_Germline) %>%
    rename(Germline = Germline_bin) %>%
    pivot_longer(
      cols = c(Reimbursed, Germline, CUP_algorithm, Clinically_relevant),
      names_to = "Parameter",
      values_to = "Status"
    ) %>%
    mutate(
      Parameter   = factor(Parameter, levels = rev(parameter_order)),
      Status      = factor(Status),
      Tile_Border = ifelse(Parameter == "Germline" & !Has_Germline, NA, "lightgrey")
    )
  
  ggplot(df_long, aes(x = Number, y = Parameter, fill = interaction(Parameter, Status))) +
    geom_tile(aes(color = Tile_Border), width = 0.95, height = 0.95, linewidth = 0.1) +
    scale_fill_manual(values = palette, guide = "none") +
    scale_color_identity(na.value = "white") +
    theme_minimal() +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y  = element_blank(),
      panel.grid   = element_blank(),
      plot.margin  = margin(10, 10, 10, 10)
    )
}

# Order
order_known <- c("111","110","101","100","011","010","001","000")
order_cup   <- c("111","101","011","001","110","100","010","000")

# Aesthetics
palette_known <- c(
  "Reimbursed.0" = "white", "Reimbursed.1" = "#7FC8F2",
  "Germline.0" = "white", "Germline.1" = "#FFB347",
  "CUP_algorithm.0" = "white", "CUP_algorithm.1" = "#B364D9",
  "Clinically_relevant.0" = "darkgrey", "Clinically_relevant.1" = "#7ED957"
)

palette_cup <- c(
  "CUP_algorithm.0" = "white", "CUP_algorithm.1" = "#B364D9",
  "Reimbursed.0" = "white", "Reimbursed.1" = "#7FC8F2",
  "Germline.0" = "white", "Germline.1" = "#FFB347",
  "Clinically_relevant.0" = "darkgrey", "Clinically_relevant.1" = "#7ED957"
)

params_known <- c("Reimbursed","Germline","CUP_algorithm","Clinically_relevant")
params_cup   <- c("CUP_algorithm","Reimbursed","Germline","Clinically_relevant")

p_known <- make_tile_panel(
  df_all          = df,
  diagnosis_label = "Known primary tumour",
  order_vec       = order_known,
  parameter_order = params_known,
  palette         = palette_known
)

p_cup <- make_tile_panel(
  df_all          = df,
  diagnosis_label = "Cancer of unknown primary",
  order_vec       = order_cup,
  parameter_order = params_cup,
  palette         = palette_cup
)

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  "output/figure4_panelA_known.pdf", 
  p_known, 
  width = 10, 
  height = 1.5, 
  units = "in")

ggsave("output/figure4_panelB_cup.pdf",   
  p_cup,   
  width = 10, 
  height = 1.5, 
  units = "in")
