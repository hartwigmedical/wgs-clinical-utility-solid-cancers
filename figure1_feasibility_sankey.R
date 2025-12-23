# figure1_feasibility_sankey.R
# Environment: 
#     R version 4.4.2
#     RStudio 2024.09.1+394
# Required packages: 
#     readxl      (1.4.5)
#     networkD3   (0.4)
#     htmlwidgets (1.6.4)
#     stringr     (1.5.1)
# Input: 
#     data/SourceData_Main+ED.xlsx, sheet: Fig1
# Output: output/Figure1_sankey_raw.html 
#     HTML output was exported to SVG
#     Final node positions were aligned manually and labels were added in Inkscape before submission as pdf

library(networkD3)
library(htmlwidgets)
library(readxl)
library(stringr)

# Input
fig1 <- read_excel("data/SourceData_Main+ED.xlsx", sheet = "Fig1")

# Nodes
nodes <- data.frame(
  name  = fig1$name,
  group = fig1$group,
  stringsAsFactors = FALSE
)

pat <- "\\((\\d+)\\)\\s*:\\s*([0-9]+(?:\\.[0-9]+)?)"

links_list <- lapply(seq_len(nrow(fig1)), function(i) {
  txt <- fig1$outgoing_breakdown[i]
  if (is.na(txt) || txt == "") return(NULL)
  
  m <- str_match_all(txt, pat)[[1]]
  if (nrow(m) == 0) return(NULL)
  
  data.frame(
    source = as.integer(fig1$id[i]),
    target = as.integer(m[, 2]),
    value  = as.numeric(m[, 3]),
    stringsAsFactors = FALSE
  )
})

links <- do.call(rbind, links_list)

# Aesthetics 
node_colour_format <- "d3.scaleOrdinal()
  .domain(['dropout', 'stage', 'fail', 'CUP', 'WGS', '523gene', '50gene'])
  .range(['#e6e6e6', '#a5dbfa', '#cb3535', '#c992e6', '#800080', '#ffd42a', '#c8e9fc'])"

# Sankey
sankey <- sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name",
  NodeGroup = "group",
  colourScale = node_colour_format,
  sinksRight = FALSE,
  fontSize = 0,
  fontFamily = "Helvetica",
  nodeWidth = 20,
  nodePadding = 28,
  width = 700,
  height = 400
)

saveWidget(
  sankey, 
  "output/Figure1_feasibility_sankey_raw.html", 
  selfcontained = TRUE)

