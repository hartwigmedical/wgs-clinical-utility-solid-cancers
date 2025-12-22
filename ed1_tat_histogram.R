# ed1_tat_histogram.R
# Environment:
#   R version 4.4.2
#   RStudio 2024.09.1+394
# Required packages :
#   readxl     (1.4.5) 
#   dplyr      (1.1.4)
#   ggplot2    (3.5.2)
# Input:
#   data/SourceData_Main+ED.xlsx, sheet: ED1
# Output:
#   output/ED1_tat_histogram.pdf  

library(readxl)
library(dplyr)
library(ggplot2)

# Input
df <- read_excel(
  path  = "data/SourceData_Main+ED.xlsx",
  sheet = "ED1"
) %>%
  mutate(
    Working_days = as.integer(Working_days),
    N_samples    = as.integer(N_samples),
    Group = ifelse(Working_days <= 10, "<=10 working days", ">10 working days")
  )

# Plot
p <- ggplot(df, aes(x = factor(Working_days), y = N_samples, fill = Group)) +
  geom_col(width = 0.85, colour = NA) +
  geom_text(
    aes(label = ifelse(N_samples > 0, N_samples, "")),
    vjust = -0.25,
    fontface = "bold",
    size = 4,
    family = "Helvetica",
    colour = "black"
  ) +
  scale_fill_manual(values = c("<=10 working days" = "#00C853", ">10 working days" = "#D32F2F")) +
  labs(
    x = "Working days",
    y = "Number\nof samples"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, family = "Helvetica"),
    axis.text.y = element_text(size = 11, family = "Helvetica"),
    axis.title.x = element_text(size = 12, family = "Helvetica", hjust = 1),
    axis.title.y = element_text(size = 12, family = "Helvetica", angle = 0, vjust = 0.85),
    plot.margin = margin(5.5, 10, 5.5, 5.5, unit = "pt")
  ) +
  expand_limits(y = max(df$N_samples, na.rm = TRUE) * 1.12)

print(p)

# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  filename = "output/ED1_tat_histogram.pdf",
  plot = p,
  width = 7.0,
  height = 5.0,
  units = "in"
)
