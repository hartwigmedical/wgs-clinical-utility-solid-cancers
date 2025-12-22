# figure5C_os_pretreatments_split.R
# Environment: R 4.4.2; RStudio 2024.09.1+394
# Required packages: 
#    readxl     (1.4.5) 
#    dplyr      (1.1.4)
#    survival   (3.7-0)
#    survminer  (0.5.0)
#    ggplot2    (3.5.2)
#    patchwork  (1.3.0)
# Input:  
#    data/SourceData_Main+ED.xlsx 
#    sheet "Fig5"
# Output:
#   output/Figure5C_os_pretreatments_split.pdf
# Notes on figure assembly:
#     The curves were assembled into the final multi-panel figure in Inkscape; only label positioning was adjusted manually for legibility

library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)
library(patchwork)

hr_line <- function(model, coef_name, label) {
  s  <- summary(model)
  hr <- s$coefficients[coef_name, "exp(coef)"]
  ci <- s$conf.int[coef_name, c("lower .95", "upper .95")]
  paste0(
    label, ": HR = ",
    sprintf("%.2f", hr),
    " (", sprintf("%.2f", ci[1]), "–", sprintf("%.2f", ci[2]), ")"
  )
}

ceil_day <- function(x) ifelse(is.na(x), NA_real_, ceiling(x - 1e-12))

make_km_one <- function(df_sub, pret_label, out_stub, show_legend = FALSE, show_y = TRUE) {
  
  fit <- survfit(Surv(Overall_survival_days, Event) ~ KM_Group, data = df_sub)
  
  df1 <- df_sub %>% mutate(KM_Group = relevel(KM_Group, ref = "BIT–Rx–"))
  model1 <- coxph(Surv(Overall_survival_days, Event) ~ KM_Group, data = df1)
  txt_ab <- hr_line(model1, "KM_GroupBIT–Rx+", "BIT–Rx+ vs BIT–Rx–")
  txt_ac <- hr_line(model1, "KM_GroupBIT+",    "BIT+ vs BIT–Rx–")
  
  df3 <- df_sub %>% mutate(KM_Group = relevel(KM_Group, ref = "BIT–Rx+"))
  model3 <- coxph(Surv(Overall_survival_days, Event) ~ KM_Group, data = df3)
  txt_bc <- hr_line(model3, "KM_GroupBIT+", "BIT+ vs BIT–Rx+")
  
  km_plot <- ggsurvplot(
    fit,
    data = df_sub,
    conf.int = TRUE,
    risk.table = TRUE,
    risk.table.y.text.col = TRUE,
    risk.table.title = "Number at risk",
    pval = FALSE,
    break.time.by = 180,
    risk.table.height = 0.25,
    palette = c("#fee090", "#fdb863", "#60bd68"),
    legend.title = "",
    legend.labs = c("BIT–Rx–", "BIT–Rx+", "BIT+"),
    ggtheme = theme_minimal(base_size = 12),
    xlab = "Days since WGS result",
    ylab = "Overall survival probability",
    legend = "bottom" 
  )
  
  km_plot$table <- km_plot$table +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      text = element_text(color = "black")
    )
  
  km_plot$plot <- km_plot$plot +
    theme(
      panel.grid.minor = element_blank(),
      legend.key = element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    ) +
    labs(title = paste0("Pretreatments: ", pret_label)) +
    annotate("text", x = 300, y = 0.94, label = txt_bc, hjust = 0, size = 4) +
    annotate("text", x = 300, y = 0.88, label = txt_ac, hjust = 0, size = 4) +
    annotate("text", x = 300, y = 0.82, label = txt_ab, hjust = 0, size = 4)
  
  if (!show_y) {
    km_plot$plot <- km_plot$plot +
      theme(
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  
# Statistics: medians
  meds <- surv_median(fit)
  strata_col <- if ("strata" %in% names(meds)) "strata" else "group"
  
  med_annot <- meds %>%
    transmute(
      strata = gsub("^KM_Group=", "", .data[[strata_col]]),
      x      = median,
      label  = ifelse(is.na(median), NA_character_, paste0(ceil_day(median), "d"))
    ) %>%
    filter(!is.na(x), strata %in% levels(df_sub$KM_Group)) %>%
    mutate(
      strata = factor(strata, levels = levels(df_sub$KM_Group)),
      x_orb = x
    ) %>%
    arrange(x_orb)

# Aesthetics
  y_median <- 0.50
  y_orb    <- 0.00  
  y_label  <- 0.06
 
  orb_cols <- c("BIT–Rx–" = "#fee090", "BIT–Rx+" = "#fdb863", "BIT+" = "#60bd68")
   
  km_plot$plot <- km_plot$plot +
    coord_cartesian(clip = "off") +
    geom_segment(
      data = med_annot,
      aes(x = 0, xend = x, y = y_median, yend = y_median),
      inherit.aes = FALSE,
      linetype = "dotted",
      colour = "black",
      linewidth = 0.4,
      show.legend = FALSE
    ) +
    geom_segment(
      data = med_annot,
      aes(x = x, xend = x, y = 0, yend = y_median),
      inherit.aes = FALSE,
      linetype = "dotted",
      colour = "black",
      linewidth = 0.4,
      show.legend = FALSE
    ) +
    geom_point(
      data = med_annot,
      aes(x = x, y = y_orb, colour = strata),
      inherit.aes = FALSE,
      shape = 16,        
      size = 5,
      show.legend = FALSE
    ) +
    geom_text(
      data = med_annot,
      aes(x = x + 20, y = y_label, label = label),
      inherit.aes = FALSE,
      colour = "black",
      fontface = "italic",
      angle = 45,
      hjust = 0,
      size = 3.6,
      family = "Helvetica",
      show.legend = FALSE
    )
  km_plot$plot
}

# Input
df <- read_excel(
  path  = "data/SourceData_Main+ED.xlsx",
  sheet = "Fig5"
) %>%
  mutate(
    Overall_survival_days = as.numeric(Overall_survival_days),
    Event = as.integer(Event),
    Actionable_biomarkers = as.integer(Actionable_biomarkers),
    Pretreatments = as.integer(Pretreatments),
    Posttreatments = as.integer(Posttreatments),
    Biomarker_informed_posttreatment = toupper(as.character(Biomarker_informed_posttreatment))
  ) %>%
  filter(
    !is.na(Overall_survival_days),
    Overall_survival_days >= 0,
    !is.na(Event),
    Actionable_biomarkers > 0
  ) %>%
  mutate(
    Pretreatment_Group = case_when(
      Pretreatments == 0 ~ "0",
      Pretreatments == 1 ~ "1",
      Pretreatments >= 2 ~ "2+",
      TRUE ~ NA_character_
    ),
    KM_Group = case_when(
      Biomarker_informed_posttreatment == "NO"  & Posttreatments == 0 ~ "BIT–Rx–",
      Biomarker_informed_posttreatment == "NO"  & Posttreatments >  0 ~ "BIT–Rx+",
      Biomarker_informed_posttreatment == "YES"                         ~ "BIT+",
      TRUE ~ NA_character_
    ),
    KM_Group = factor(KM_Group, levels = c("BIT–Rx–", "BIT–Rx+", "BIT+"))
  ) %>%
  filter(!is.na(Pretreatment_Group), !is.na(KM_Group))

# Plot
p0 <- make_km_one(df %>% filter(Pretreatment_Group == "0"),  "0",  "Figure5C_pretx0",
                  show_legend = FALSE, show_y = TRUE)

p1 <- make_km_one(df %>% filter(Pretreatment_Group == "1"),  "1",  "Figure5C_pretx1",
                  show_legend = TRUE,  show_y = FALSE)

p2 <- make_km_one(df %>% filter(Pretreatment_Group == "2+"), "2+", "Figure5C_pretx2plus",
                  show_legend = FALSE, show_y = FALSE)

combined <- (p0 | p1 | p2) /
  (plot_spacer() | guide_area() | plot_spacer()) +
  plot_layout(heights = c(1.5, 0), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box.just = "center",
    legend.justification = "center"
  )

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)
      
ggsave(
  "output/Figure5C_os_pretreatments_split.pdf", 
  combined,
  width = 13.5,
  height = 5,
  units = "in"
)
