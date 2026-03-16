# figure4ab_clinical_utility.R
# Environment:
#   R 4.4.2; RStudio 2024.09.1+394
# Packages: 
#   readxl  (1.4.5)
#   dplyr   (1.1.4)
#   tidyr   (1.3.1)
#   ggplot2 (3.5.2)
# Input:
#   data/SourceData_Main+ED.xlsx, sheet: Fig4
# Output:
#   A: output/figure4_panelA_known.pdf
#   B: output/figure4_panelB_cup.pdf
#   Left-side labels: output/figure4_leftlabels.pdf
#   Right-side labels: output/figure4_rightlabels.pdf
#   (Panels and separate label files were assembled into the final figure in Inkscape)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Input
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
  height = 2, 
  units = "in")

ggsave("output/figure4_panelB_cup.pdf",   
  p_cup,   
  width = 10, 
  height = 2, 
  units = "in")

# Create left-side labels
df_left <- rbind(
  data.frame(
    panel = "A",
    y = 4:1,
    label = c(
      "Reimbursed care biomarker found",
      "Pathogenic germline variant found",
      "Diagnosis clarified or changed",
      "Any clinically relevant result"
    ),
    pct = c("27%", "7%", "4%", "35%")
  ),
  data.frame(
    panel = "B",
    y = 4:1,
    label = c(
      "Diagnosis solved in patient with CUP",
      "Reimbursed care biomarker found",
      "Pathogenic germline variant found",
      "Any clinically relevant result"
    ),
    pct = c("63%", "11%", "6%", "67%")
  )
)

gap <- 2.4
df_left$y_global <- ifelse(df_left$panel == "A",
                           df_left$y + (4 + gap),   
                           df_left$y)               

df_panletters <- data.frame(
  panel = c("A", "B"),
  x = -0.35,
  y_global = c(4.55 + (4 + gap), 4.55)
)

p_left <- ggplot() +
  geom_text(data = df_left, aes(x = -0.2, y = y_global-0.4, label = label),
            hjust = 0, size = 3) +
  geom_text(data = df_left, aes(x = 6, y = y_global-0.4, label = pct),
            hjust = 1, fontface = "bold", size = 4.5) +
  geom_text(data = df_panletters, aes(x = x, y = y_global, label = panel),
            hjust = 0, vjust = 1, fontface = "bold", size = 6) +
  coord_cartesian(xlim = c(-0.4, 8.8),
                  ylim = c(0.5, (4 + gap) + 4.7),
                  clip = "off") +
  theme_void(base_family = "Helvetica")

p_left

ggsave("output/figure4_leftlabels.pdf", 
       p_left, 
       device = grDevices::cairo_pdf, 
       width = 6, 
       height = 4, 
       units = "in")


# Create right-side labels
block_gap <- 2.4
row_step  <- 0.855  
y_shift   <- -0.4

n <- 4
y_local <- rev(seq(0, (n-1) * row_step, by = row_step)) + 1  

df_right <- rbind(
  data.frame(panel="A", i=1:4, right=c("162/600","38/572","26/600","211/600")),
  data.frame(panel="B", i=1:4, right=c("77/123","14/123","7/120","83/123"))
)

df_right$y_local <- y_local[df_right$i]

block_height <- (n-1) * row_step + 1  

df_right$y_global <- ifelse(df_right$panel == "A",
                            df_right$y_local + (block_height + block_gap),
                            df_right$y_local)

x_right <- 0

p_right <- ggplot(df_right) +
  geom_text(
    aes(x = x_right, y = y_global + y_shift, label = right),
    hjust = 1, size = 2.3, family = "Helvetica"
  ) +
  coord_cartesian(
    xlim = c(-0.1, 0.1),
    ylim = c(0.5, (block_height + block_gap) + block_height + 0.7),
    clip = "off"
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4),
    text = element_text(colour = "black")
  )

ggsave("output/figure4_rightlabels.pdf", 
       p_right, 
       device = grDevices::cairo_pdf, 
       width = 1.0, 
       height = 2.9, 
       units = "in")
