# ed3_vignette_cohort_bars.R
# Environment:
#   R version 4.4.2
#   RStudio 2024.09.1+394
# Required packages :
#   readxl     (1.4.5) 
#   dplyr      (1.1.4)
#   tidyr      (1.3.1)
#   stringr    (1.5.1)
#   ggplot2    (3.5.2)
#   scales     (1.3.0)
# Input:
#   data/SourceData_Main+ED.xlsx, sheet: ED5
# Output:
#   output/ed3_vignette_cohort_bars.pdf  

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Input
df <- read_excel("data/SourceData_Main+ED.xlsx", sheet = "ED3")

# Define
df_long <- bind_rows(
  df %>%
    transmute(
      cohort = "Study cohort",
      tumour_type = Study_cohort,
      n = as.integer(N_study)
    ),
  df %>%
    transmute(
      cohort = "Control cohort",
      tumour_type = Control_cohort,
      n = as.integer(N_control)
    )
) %>%
  filter(!is.na(tumour_type), !is.na(n), n > 0) %>%
  mutate(
    tumour_type = str_squish(as.character(tumour_type)),
    cohort = factor(cohort, levels = c("Control cohort", "Study cohort"))
  ) %>%
  group_by(cohort) %>%
  arrange(tumour_type, .by_group = TRUE) %>%
  ungroup()

tum_levels <- sort(unique(df_long$tumour_type))

tum_map <- tibble(
  tumour_type = tum_levels,
  fill_pos = if (length(tum_levels) == 1) 0.5 else (seq_along(tum_levels) - 1) / (length(tum_levels) - 1)
)

df_long <- df_long %>%
  mutate(tumour_type = factor(tumour_type, levels = tum_levels)) %>%
  left_join(tum_map, by = "tumour_type")

tiles <- df_long %>%
  group_by(cohort, tumour_type) %>%
  tidyr::uncount(weights = n, .id = "i_within") %>%
  ungroup() %>%
  group_by(cohort) %>%
  mutate(
    x = row_number(),
    total = max(x)
  ) %>%
  ungroup()

blocks <- tiles %>%
  group_by(cohort, tumour_type) %>%
  summarise(
    xmin = min(x) - 0.5,
    xmax = max(x) + 0.5,
    n = dplyr::n(),
    total = max(total),
    .groups = "drop"
  ) %>%
  mutate(
    pct = n / total,
    xmid = (xmin + xmax) / 2,
    label_n = paste0(n, " (", percent(pct, accuracy = 1), ")")
  ) %>%
  group_by(cohort) %>%
  mutate(show_label = rank(-n, ties.method = "first") <= 5) %>%
  ungroup()

# Aesthetics
grad_cols <- c("#C77CFF", "#5A43FF", "#0033CC", "#00B050", "#00FF00")

# Plot
p <- ggplot(tiles, aes(x = x, y = 0, fill = fill_pos)) +
  geom_tile(width = 1, height = 0.9) +
  geom_rect(
    data = blocks,
    aes(xmin = xmin, xmax = xmax, ymin = -0.45, ymax = 0.45),
    inherit.aes = FALSE,
    fill = NA, colour = "grey90", linewidth = 0.2
  ) +
  geom_rect(
    data = blocks %>% filter(show_label),
    aes(xmin = xmin, xmax = xmax, ymin = 0.46, ymax = 1.08),
    inherit.aes = FALSE,
    fill = "grey90", colour = "grey90"
  ) +
  geom_rect(
    data = blocks %>% filter(show_label),
    aes(xmin = xmin, xmax = xmax, ymin = -0.46, ymax = 1.06),
    inherit.aes = FALSE,
    fill = "NA", colour = "grey90", linewidth = 0.6
  ) +
  geom_text(
    data = blocks %>% filter(show_label),
    aes(x = xmid, y = 0.9, label = as.character(tumour_type)),
    inherit.aes = FALSE,
    fontface = "bold", size = 4
  ) +
  geom_text(
    data = blocks %>% filter(show_label),
    aes(x = xmid, y = 0.65, label = label_n),
    inherit.aes = FALSE,
    size = 3.6
  ) +
  facet_wrap(~ cohort, ncol = 1, scales = "free_x") +
  scale_fill_gradientn(colours = grad_cols, guide = "none") +
  coord_cartesian(ylim = c(-0.55, 1.2), clip = "off") +
  theme_minimal(base_size = 12, base_family = "Helvetica") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  "output/ed3_vignette_cohort_bars.pdf", 
  p, 
  width = 14, 
  height = 5, 
  units = "in")
