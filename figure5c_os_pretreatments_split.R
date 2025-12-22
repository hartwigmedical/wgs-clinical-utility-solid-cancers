# figure5C_pretreatment_stratified.R
# Purpose: Figure 5C — OS for actionable biomarkers >0, targeted pretreatment = NO,
#          stratified by pretreatments (0 / 1 / 2+), with groups Rx– / Rx+ / TargetRx+.
# Input:  data/SourceData_Main+ED.xlsx, sheet "Fig5"
# Output: output/Figure5C_pretx0_curve.pdf + _risktable.pdf (idem for pret1 and pret2plus)
#         output/Figure5C_combined_curves.pdf  (CURVES ONLY; risk tables separate)

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
  paste0(label, ": HR = ",
         sprintf("%.2f", hr),
         " (", sprintf("%.2f", ci[1]), "–", sprintf("%.2f", ci[2]), ")")
}

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
      text = element_text(color = "black")
    )
  
  km_plot$plot <- km_plot$plot +
    theme(
      panel.grid.minor = element_blank(),
      legend.key = element_blank(),
      legend.position = "none"
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
    
    ceil_day <- function(x) ifelse(is.na(x), NA_real_, ceiling(x - 1e-12))
    
    curve_palette <- c(
      "BIT–Rx–" = "#fee090",
      "BIT–Rx+" = "#fdb863",
      "BIT+"    = "#60bd68"
    )
    
    med_annot <- surv_median(fit) %>%
      dplyr::transmute(
        strata = gsub("^KM_Group=", "", strata),
        x      = median,
        label  = ifelse(is.na(median), NA_character_, paste0(ceil_day(median), "d"))
      ) %>%
      dplyr::filter(!is.na(x), strata %in% levels(df_sub$KM_Group)) %>%
      dplyr::mutate(strata = factor(strata, levels = levels(df_sub$KM_Group)),
                    x_orb = x + dplyr::case_when(
                      strata == "BIT–Rx–" ~ -8,
                      strata == "BIT–Rx+" ~  0,
                      strata == "BIT+"    ~  8,
                      TRUE ~ 0
                    )
      ) %>%
      dplyr::arrange(x_orb)
    
    y_median <- 0.50
    y_orb    <- 0.001
    y_label  <- 0.04
    
    km_plot$plot <- km_plot$plot +
      geom_segment(
        data = med_annot,
        aes(x = 0, xend = x, y = y_median, yend = y_median),
        linetype = "dotted", colour = "black",
        inherit.aes = FALSE
      ) +
      geom_segment(
        data = med_annot,
        aes(x = x, xend = x, y = 0, yend = y_median),
        linetype = "dotted", colour = "black",
        inherit.aes = FALSE
      ) +
      geom_point(
        data = med_annot,
        aes(x = x, y = y_orb, fill = strata),
        shape = 21, size = 3.6, stroke = 0,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      scale_fill_manual(values = curve_palette, guide = "none") +
      geom_text(
        data = med_annot,
        aes(x = x + 20, y = y_label, label = label),
        colour = "black",
        fontface = "italic",
        angle = 45,
        hjust = 0,
        size = 3.8,
        family = "Helvetica",
        inherit.aes = FALSE
      )
    
  }
  
  km_plot$plot
}

df <- read_excel(
  path  = "~/Documents/Post-WIDE/SourceData_Main+ED.xlsx",
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

p0 <- make_km_one(df %>% filter(Pretreatment_Group == "0"),  "0",  "Figure5C_pretx0", show_y = TRUE)
p1 <- make_km_one(df %>% filter(Pretreatment_Group == "1"),  "1",  "Figure5C_pretx1", show_y = FALSE)
p2 <- make_km_one(df %>% filter(Pretreatment_Group == "2+"), "2+", "Figure5C_pretx2plus", show_y = FALSE)

combined <- (p0 | p1 | p2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box.just = "center",
    legend.justification = "center"
  )

combined

if (!dir.exists("output")) dir.create("output", recursive = TRUE)
ggsave("output/Figure5C_combined_curves.pdf", combined,
       width = 13.5, height = 5, units = "in")
)

