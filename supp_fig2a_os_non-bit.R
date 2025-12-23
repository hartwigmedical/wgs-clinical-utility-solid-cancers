# supp_fig2a_os_non-bit.R
# Environment: R 4.4.2; RStudio 2024.09.1+394
# Required packages: 
#    readxl     (1.4.5) 
#    dplyr      (1.1.4)
#    survival   (3.7-0)
#    survminer  (0.5.0)
#    ggplot2    (3.5.2)
# Input: 
#    data/SourceData_Main+ED.xlsx, sheet "SuppFig2"
# Output:
#   output/SuppFig2a_os_non-bit.pdf
# Notes on figure assembly:
#     The curves were assembled into the final multi-panel figure in Inkscape; only label positioning was adjusted manually for legibility

library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)

pairwise_hr <- function(model, L, digits = 2) {
  b <- coef(model); V <- vcov(model)
  est <- as.numeric(t(L) %*% b)
  se  <- sqrt(as.numeric(t(L) %*% V %*% L))
  HR  <- round(exp(est), digits)
  L95 <- round(exp(est - 1.96 * se), digits)
  U95 <- round(exp(est + 1.96 * se), digits)
  c(HR = HR, L95 = L95, U95 = U95)
}

ceil_day <- function(x) ifelse(is.na(x), NA_real_, ceiling(x - 1e-12))

# Input
df <- read_excel(
  path  = "~/Documents/Post-WIDE/SourceData_Main+ED.xlsx",
  sheet = "SuppFig2"
) %>%
  mutate(
    Overall_survival_days = as.numeric(Overall_survival_days),
    Event = as.integer(Event),
    Actionable_biomarkers = as.integer(Actionable_biomarkers),
    Biomarker_informed_pretreatment = toupper(as.character(Biomarker_informed_pretreatment)),
    Posttreatments = as.integer(Posttreatments),
    Biomarker_informed_posttreatment = toupper(as.character(Biomarker_informed_posttreatment))
  ) %>%
  filter(Biomarker_informed_pretreatment == "NO") %>%
  mutate(
    KM_Group = case_when(
      Actionable_biomarkers == 0 ~ "Actionable–",
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "NO"  & Posttreatments == 0 ~ "BIT–Rx–",
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "NO"  & Posttreatments >  0 ~ "BIT–Rx+",
      Actionable_biomarkers > 0 & Biomarker_informed_posttreatment == "YES"                      ~ "BIT+",
      TRUE ~ NA_character_
    ),
    KM_Group = factor(KM_Group, levels = c("Actionable–", "BIT–Rx–", "BIT–Rx+", "BIT+"))
  ) %>%
  filter(
    !is.na(Overall_survival_days), Overall_survival_days >= 0,
    !is.na(Event),
    !is.na(KM_Group)
  )

# Statistics: Cox
fit <- survfit(Surv(Overall_survival_days, Event) ~ KM_Group, data = df)

df_refA <- df %>% mutate(KM_Group = relevel(KM_Group, ref = "Actionable–"))
model <- coxph(Surv(Overall_survival_days, Event) ~ KM_Group, data = df_refA)

hr_DC <- pairwise_hr(model, c( 0, -1,  1)) # BIT+ vs BIT–Rx+
hr_DB <- pairwise_hr(model, c(-1,  0,  1)) # BIT+ vs BIT–Rx–
hr_CB <- pairwise_hr(model, c(-1,  1,  0)) # BIT–Rx+ vs BIT–Rx–
hr_BA <- pairwise_hr(model, c( 1,  0,  0)) # BIT–Rx– vs Actionable–
hr_CA <- pairwise_hr(model, c( 0,  1,  0)) # BIT–Rx+ vs Actionable–
hr_DA <- pairwise_hr(model, c( 0,  0,  1)) # BIT+ vs Actionable–

txt <- paste(
  paste0("BIT+ vs BIT–Rx+: HR = ", hr_DC["HR"], " (", hr_DC["L95"], "–", hr_DC["U95"], ")"),
  paste0("BIT+ vs BIT–Rx–: HR = ", hr_DB["HR"], " (", hr_DB["L95"], "–", hr_DB["U95"], ")"),
  paste0("BIT–Rx+ vs BIT–Rx–: HR = ", hr_CB["HR"], " (", hr_CB["L95"], "–", hr_CB["U95"], ")"),
  "",
  paste0("BIT–Rx– vs Actionable–: HR = ", hr_BA["HR"], " (", hr_BA["L95"], "–", hr_BA["U95"], ")"),
  paste0("BIT–Rx+ vs Actionable–: HR = ", hr_CA["HR"], " (", hr_CA["L95"], "–", hr_CA["U95"], ")"),
  paste0("BIT+ vs Actionable–: HR = ",    hr_DA["HR"], " (", hr_DA["L95"], "–", hr_DA["U95"], ")"),
  sep = "\n"
)

# Aesthetics
curve_cols <- c(
  "Actionable–" = "grey40",
  "BIT–Rx–"     = "#fee090",
  "BIT–Rx+"     = "#fdb863",
  "BIT+"        = "#60bd68"
)

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
  palette = unname(curve_cols),
  legend.title = "",
  legend.labs = levels(df$KM_Group),
  ggtheme = theme_minimal(base_size = 12),
  xlab = "Days since WGS result",
  ylab = "Overall survival probability",
  legend = "right"
)

# Statistics: medians
med_df <- surv_median(fit) %>%
  transmute(
    KM_Group = gsub("^KM_Group=", "", strata),
    x = median,
    y = 0.5
  ) %>%
  filter(!is.na(x), is.finite(x), x >= 0)

orb_df <- med_df %>%
  mutate(
    y_orb = 0.00,
    x_lab = ceiling(x - 1e-12),        
    label = paste0(x_lab, "d")
  )

km_plot$plot <- km_plot$plot +
  geom_segment(
    data = med_df,
    aes(x = 0, xend = x, y = y, yend = y),
    inherit.aes = FALSE,
    linetype = "dotted",
    colour = "black",
    linewidth = 0.6
  ) +
  geom_segment(
    data = med_df,
    aes(x = x, xend = x, y = 0, yend = y),
    inherit.aes = FALSE,
    linetype = "dotted",
    colour = "black",
    linewidth = 0.6
  ) +
  geom_point(
    data = orb_df,
    aes(x = x, y = y_orb, colour = KM_Group),
    inherit.aes = FALSE,
    shape = 16,   
    size = 4,
    show.legend = FALSE
  ) +
  geom_text(
    data = orb_df,
    aes(x = x + 12, y = y_orb + 0.03, label = label),
    inherit.aes = FALSE,
    fontface = "italic",
    angle = 45,
    family = "Helvetica",
    size = 4,
    colour = "black",
    hjust = 0
  ) 
 
 km_plot$plot <- km_plot$plot +
  annotate(
    "text",
    x = 800, y = 0.96,          
    label = txt,
    hjust = 0, vjust = 1,
    size = 3.8,
    lineheight = 1.1,
    colour = "black"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.key = element_blank()
  ) +
  coord_cartesian(clip = "off")

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  "output/SuppFig2A_os_non-bit.pdf", 
  km_plot$plot, 
  width = 9, 
  height = 4, 
  units = "in")
ggsave(
  "output/SuppFig2A_os_non-bit_risktable.pdf", 
  km_plot$table, 
  width = 9, 
  height = 1.6, 
  units = "in")
