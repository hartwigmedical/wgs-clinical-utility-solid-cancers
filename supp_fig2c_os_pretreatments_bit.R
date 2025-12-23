library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)

# Input
df <- read_excel(
  "data/SourceData_Main+ED.xlsx",
  sheet = "SuppFig2"
) %>%
  filter(Actionable_biomarkers > 0) %>%
  mutate(
    Overall_survival_days = as.numeric(Overall_survival_days),
    Event = as.integer(Event),
    Pretreated = factor(ifelse(Biomarker_informed_pretreatment == "YES", "Pretreated", "Not pretreated"))
  ) %>%
  filter(
    !is.na(Overall_survival_days),
    Overall_survival_days >= 0,
    !is.na(Event),
    !is.na(Pretreated)
  ) %>%
  mutate(Pretreated = factor(Pretreated, levels = c("Not pretreated", "Pretreated")))

fit   <- survfit(Surv(Overall_survival_days, Event) ~ Pretreated, data = df)
model <- coxph(Surv(Overall_survival_days, Event) ~ Pretreated, data = df)

s  <- summary(model)
hr <- s$coefficients["PretreatedPretreated", "exp(coef)"]
ci <- s$conf.int["PretreatedPretreated", c("lower .95", "upper .95")]
txt <- paste0("HR = ", sprintf("%.2f", hr), " (", sprintf("%.2f", ci[1]), "–", sprintf("%.2f", ci[2]), ")")

curve_cols <- c("Not pretreated" = "#bababa", "Pretreated" = "#64a2c5")

p <- ggsurvplot(
  fit,
  data = df,
  conf.int = TRUE,
  pval = FALSE,
  risk.table = FALSE,
  xlab = "Days since WGS result",
  ylab = "Overall survival probability",
  legend.title = "",
  legend.labs = c("Not pretreated", "Pretreated"),
  legend = "bottom",
  palette = unname(curve_cols),
  ggtheme = theme_minimal(base_size = 11)
)

# Statistics: medians
ceil_day <- function(x) ifelse(is.na(x), NA_real_, ceiling(x - 1e-12))

med <- surv_median(fit)
med_df <- data.frame(
  Pretreated = gsub("^Pretreated=", "", med$strata),
  x = med$median
) %>%
  mutate(
    Pretreated = factor(Pretreated, levels = levels(df$Pretreated)),
    x_lab = ceil_day(x),
    label = paste0(x_lab, "d"),
    x_orb = x,
    y_line = 0.5,
    y_orb  = 0.00
  ) %>%
  filter(!is.na(x)) %>%
  arrange(x_orb)

p$plot <- p$plot +
  geom_segment(
    data = med_df,
    aes(x = 0, xend = x, y = y_line, yend = y_line),
    inherit.aes = FALSE,
    linetype = "dotted",
    colour = "black",
    linewidth = 0.6
  ) +
  geom_segment(
    data = med_df,
    aes(x = x, xend = x, y = 0, yend = y_line),
    inherit.aes = FALSE,
    linetype = "dotted",
    colour = "black",
    linewidth = 0.6
  ) +
  geom_point(
    data = med_df,
    aes(x = x, y = y_orb, colour = Pretreated),
    inherit.aes = FALSE,
    shape = 16,
    size = 5
  ) +
  scale_fill_manual(values = curve_cols, guide = "none") +
  geom_text(
    data = med_df,
    aes(x = x + 5, y = y_orb + 0.02, label = label),
    inherit.aes = FALSE,
    angle = 45,
    fontface = "italic",
    size = 4,
    hjust = -0.1,
    colour = "black"
  )

p$plot <- p$plot +
  scale_x_continuous(breaks = seq(0, 2160, by = 360)) +
  annotate("text", x = 650, y = 0.95, label = txt, hjust = 0, size = 4) +
  labs(title = "") +
  theme(panel.grid.minor = element_blank())

ggsave(
  "output/SuppFig2c_pretreatments_bit.pdf",
  p$plot, 
  width = 4.0, 
  height = 4.0, 
  units = "in")
