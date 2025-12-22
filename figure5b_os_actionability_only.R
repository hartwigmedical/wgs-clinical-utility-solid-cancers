# figure5B_os_actionable_only.R
# Environment: R 4.4.2; RStudio 2024.09.1+394
# Required packages: 
#    readxl     (1.4.5) 
#    dplyr      (1.1.4)
#    survival   (3.7-0)
#    survminer  (0.5.0)
#    ggplot2    (3.5.2)
# Input:
#    data/SourceData_Main+ED.xlsx, sheet: Fig5
# Output:
#     output/figure5b_os_actionable_only.pdf 
#     output/figure5b_os_actionable_only_risktable.pdf 
# Notes on figure assembly:
#     The curve and risk table were exported separately as vector PDFs and assembled into the final multi-panel figure in Inkscape.

library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)

ceil_day <- function(x) ifelse(is.na(x), NA_real_, ceiling(x - 1e-12))

# Input
df <- read_excel(
  path  = "data/SourceData_Main+ED.xlsx",
  sheet = "Fig5"
) %>%
  mutate(
    Overall_survival_days = as.numeric(Overall_survival_days),
    Event = as.integer(Event),
    Actionable_biomarkers = as.integer(Actionable_biomarkers),
    Biomarker_informed_posttreatment = toupper(as.character(Biomarker_informed_posttreatment)),
    Posttreatments = as.integer(Posttreatments)
  ) %>%
  filter(
    !is.na(Overall_survival_days), Overall_survival_days >= 0,
    !is.na(Event),
    !is.na(Actionable_biomarkers), Actionable_biomarkers >= 1
  ) %>%
  mutate(
    KM_Group = case_when(
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "NO"  & Posttreatments == 0 ~ "BIT-Rx–",
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "NO"  & Posttreatments > 0  ~ "BIT-Rx+",
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "YES"                        ~ "BIT+",
      TRUE ~ NA_character_
    ),
    KM_Group = factor(KM_Group, levels = c("BIT-Rx–", "BIT-Rx+", "BIT+"))
  ) %>%
  filter(!is.na(KM_Group))

fit <- survfit(Surv(Overall_survival_days, Event) ~ KM_Group, data = df)

# Statistics: median
meds <- surv_median(fit)

medians_df <- meds %>%
  transmute(
    strata = gsub("^KM_Group=", "", strata),
    x = median,
    label = ifelse(is.na(median), "NR", paste0(ceil_day(median), "d"))
  ) %>%
  filter(strata %in% levels(df$KM_Group)) %>%
  mutate(
    strata = factor(strata, levels = levels(df$KM_Group))
  )

hr_line <- function(model, coef_name, label) {
  s <- summary(model)
  hr <- sprintf("%.2f", s$coefficients[coef_name, "exp(coef)"], 2)
  ci <- sprintf("%.2f", s$conf.int[coef_name, c("lower .95", "upper .95")], 2)
  paste0(label, ": HR = ", hr, " (", ci[1], "–", ci[2], ")")
}

# Rx+ vs Rx–; TargetRx+ vs Rx– (ref = BIT-Rx–)
df1 <- df %>% mutate(KM_Group = relevel(KM_Group, ref = "Rx–"))
model1 <- coxph(Surv(Overall_survival_days, Event) ~ KM_Group, data = df1)
txt_ab <- hr_line(model1, "KM_GroupBIT-Rx+",       "BIT-Rx+ vs BIT-Rx–")
txt_ac <- hr_line(model1, "KM_GroupBIT+", "BIT+ vs BIT-Rx–")

# TargetRx+ vs Rx+ (ref = BIT-Rx+)
df3 <- df %>% mutate(KM_Group = relevel(KM_Group, ref = "BIT-Rx+"))
model3 <- coxph(Surv(Overall_survival_days, Event) ~ KM_Group, data = df3)
txt_bc <- hr_line(model3, "KM_GroupBIT+", "BIT+ vs BIT-Rx+")

# Plot
km_plot <- ggsurvplot(
  fit,
  data = df,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.y.text.col = TRUE,
  risk.table.title = "Number at risk",
  pval = FALSE,
  break.time.by = 180,
  risk.table.height = 0.25,
  palette = c("#fee090", "#fdb863", "#60bd68"),
  legend.title = "",
  legend.labs = c("Rx–", "Rx+", "TargetRx+"),
  ggtheme = theme_minimal(base_size = 12),
  xlab = "Days since WGS result",
  ylab = "Overall survival probability",
  legend = "right"
)

km_plot$table <- km_plot$table +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"),
    text = element_text(color = "black")
  )

y_orb <- 0.00

km_plot$plot <- km_plot$plot +
  theme(panel.grid.minor = element_blank()) +
  geom_segment(
    data = medians_df,
    aes(x = x, xend = x, y = 0, yend = 0.5),
    linetype = "dotted", colour = "black",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = medians_df,
    aes(x = x, y = y_orb, colour = strata),
    size = 5,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(
    data = medians_df,
    aes(x = x + 40, y = 0.07, label = label),
    colour = "black",
    fontface = "italic",
    angle = 45,
    vjust = 1,
    family = "Helvetica",
    inherit.aes = FALSE
  ) +
  annotate("text", x = 300, y = 0.94, label = txt_bc, hjust = 0, size = 4) +
  annotate("text", x = 300, y = 0.88, label = txt_ac, hjust = 0, size = 4) +
  annotate("text", x = 300, y = 0.82, label = txt_ab, hjust = 0, size = 4) +
  scale_colour_manual(values = c("BIT-Rx–" = "#fee090", "BIT-Rx+" = "#fdb863", "BIT+" = "#60bd68"))

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)
ggsave(
  "output/Figure5B_os_actionable_only.pdf", 
  km_plot$plot, 
  width = 7.0, 
  height = 5.0, 
  units = "in")
ggsave(
  "output/Figure5B_os_actionable_only_risktable.pdf", 
  km_plot$table, 
  width = 7.0, 
  height = 5.0, 
  units = "in")
