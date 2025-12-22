library(ggplot2)
library(dplyr)
library(readxl)

# Input
df <- read_excel("~/Documents/Post-WIDE/SourceData_Main+ED.xlsx", sheet = "ED5")
  data <- df %>%
  select("Tumour_type", "Gene", "Zygosity") 
  
data_summary <- data %>%
  group_by(Tumour_type, Gene, Zygosity) %>%
  summarise(Count = n(), .groups = "drop")

data_summary <- data_summary %>%
  mutate(Size = 6 + (Count - 1) * 2)

# Order
data_summary <- data_summary %>%
  mutate(
    Tumour_type = factor(Tumour_type, levels = sort(unique(Tumour_type))),
    Gene = factor(Gene, levels = rev(sort(unique(Gene))))
  )

# Aesthetics
data_summary <- data_summary %>%
  group_by(Tumour_type, Gene) %>%
  mutate(
    Offset = if_else(duplicated(Size) | duplicated(Size, fromLast = TRUE),
                     c(-0.15, 0.15)[1:n()], 0)
  ) %>%
  ungroup()

variant_colors <- c(
  "Mono-allelic" = "#FFCC80",   
  "Biallelic" = "#FF8C00"
)

# Plot
ggplot(data_summary, aes(x = as.numeric(Tumour_type) + Offset, y = Gene)) +
  geom_point(aes(size = Size, fill = Zygosity),
             shape = 21, alpha = 1, stroke = 0) +
  scale_size_continuous(
    name = "Frequency",
    range = c(6, max(data_summary$Size)),
    breaks = sort(unique(data_summary$Size)),
    labels = sort(unique(data_summary$Count))
  ) +
  scale_fill_manual(
    values = variant_colors
  ) +
  scale_x_continuous(
    breaks = seq_along(levels(data_summary$Tumour_type)),
    labels = levels(data_summary$Tumour_type)
  ) +
  guides(
    size = guide_legend(
      override.aes = list(size = sort(unique(data_summary$Size)), fill = "black")
    ),
    fill = guide_legend(
      override.aes = list(size = 5)  
    )
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )


# Save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

ggsave(
  "output/ed5_germline_grid.pdf", 
  width = 7, 
  height = 5, 
  units = "in")
