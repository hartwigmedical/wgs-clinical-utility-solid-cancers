# figure5A_os_total.R
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
#     output/figure5a_overall_survival.pdf 
#     output/figure5a_overall_survival_risktable.pdf 
# Notes on figure assembly:
#     The curve and risk table were exported separately as vector PDFs and assembled into the final multi-panel figure in Inkscape.
#     Inkscape edits were layout-only (alignment, placement of median labels, addition of median “orb” markers, and styling of median guide lines). No underlying data or statistical results were altered.

library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)

# Input
df <- read_excel(
  path  = "data/SourceData_Main+ED.xlsx",
  sheet = "Fig5" 
) %>%
  mutate(
    Overall_survival_days = as.numeric(Overall_survival_days),
    Event = as.integer(Event),
    
    KM_Group = case_when(
      Actionable_biomarkers == 0 ~ "Actionable–",
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "NO"  ~ "BIT–",
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "YES" ~ "BIT+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Overall_survival_days), Overall_survival_days >= 0, !is.na(Event), !is.na(KM_Group)) %>%
  mutate(
    KM_Group = factor(KM_Group, levels = c("Actionable–", "BIT–", "BIT+"))
  )

fit <- survfit(Surv(Overall_survival_days, Event) ~ KM_Group, data = df)

# Statstics: Medians
meds <- surv_median(fit)

ceil_day <- function(x) ceiling(x - 1e-6)

medians_df <- meds %>%
  transmute(
    strata = gsub("^KM_Group=", "", strata),
    x = median,
    y = 0.5,
    label = ifelse(is.na(median), "NR", paste0(ceil_day(median), "d"))
  ) %>%
  filter(strata %in% levels(df$KM_Group)) %>%
  mutate(strata = factor(strata, levels = levels(df$KM_Group)))

# Statistics: Hazard ratio
hr_line <- function(model, coef_name, label) {
  s <- summary(model)
  hr <- round(s$coefficients[coef_name, "exp(coef)"], 2)
  ci <- round(s$conf.int[coef_name, c("lower .95", "upper .95")], 2)
  paste0(label, ": HR = ", hr, " (", ci[1], "–", ci[2], ")")
}

# Combine
# B vs A + C vs A (ref = Target–)
df1 <- df %>% mutate(KM_Group = relevel(KM_Group, ref = "Actionable–"))
model1 <- coxph(Surv(Overall_survival_days, Event) ~ KM_Group, data = df1)

txt_ab <- hr_line(model1, "KM_GroupBIT–", "BIT– vs. Actionable–")
txt_ac <- hr_line(model1, "KM_GroupBIT+", "BIT+ vs. Actionable–")

# C vs B (ref = BIT–)
df3 <- df %>% mutate(KM_Group = relevel(KM_Group, ref = "BIT–"))
model3 <- coxph(Surv(Overall_survival_days, Event) ~ KM_Group, data = df3)

txt_bc <- hr_line(model3, "KM_GroupBIT+", "BIT+ vs. BIT–")

# Aesthetics
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
  palette = c("grey40", "#fdb863", "#60bd68"),
  legend.title = "",
  legend.labs = c("Actionable–", "BIT–", "BIT+"),
  ggtheme = theme_minimal(base_size = 12),
  xlab = "Days since WGS result",
  ylab = "Overall survival probability",
  legend = "right"
)

km_plot$plot <- km_plot$plot +
  theme(panel.grid.minor = element_blank()) 

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
  ) +
  coord_cartesian(clip = "off")

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
    aes(x = x + 50, y = 0.06, label = label, colour = strata),
    fontface = "italic",
    colour = "black",
    angle = 45,
    family = "Helvetica",
    inherit.aes = FALSE
  ) +
  annotate("text", x = 300, y = 0.95, label = txt_bc, hjust = 0, size = 4) +
  annotate("text", x = 300, y = 0.88, label = txt_ac, hjust = 0, size = 4) +
  annotate("text", x = 300, y = 0.81, label = txt_ab, hjust = 0, size = 4) +
  theme(legend.key = element_blank())

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave("output/figure5a_overall_survival.pdf", 
       km_plot$plot, 
       width = 7.0, 
       height = 5.0, 
       units = "in")
ggsave("output/figure5a_overall_survival_risktable.pdf", 
       km_plot$table, 
       width = 7.0, 
       height = 2.0, 
       units = "in")
