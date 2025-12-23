# figure1_feasibility_sankey.R
# Purpose: Generates the Sankey diagram underlying Figure 1 (feasibility and panel coverage)
# Environment: 
#     R version 4.4.2
#     RStudio 2024.09.1+394
# Required packages: 
#     networkD3   (0.4)
#     htmlwidgets (1.6.4)
#     extrafont   (0.20)
# Input: 
#     None: node and link data defined in the script (counts corresponding to the curated cohort described in the manuscript)
# Output: output/Figure1_sankey_raw.html 
#     HTML output was exported to SVG
#     Final node positions were aligned manually and labels were added in Inkscape before submission as pdf

library(networkD3) 
library(extrafont)
library(htmlwidgets)

# Nodes
nodes <- data.frame(name = c(
  "WGS Requests",                       # 0
  "Successful Sampling",                # 1
  "Unsuccessful Sampling or Cancelled", # 2
  "Successful WGS",                     # 3
  "Insufficient pTCP",                  # 4
  "Insufficient mTCP",                  # 5
  "Insufficient DNA Quantity",          # 6
  "Insufficient DNA Quality",           # 7
  "Known Primary Tumour",               # 8
  "Cancer of Unknown Primary",          # 9
  "No Consent",                         #10
  "Second Successful WGS",              #11
  "No biomarkers detected",             #12
  "Only WGS",                           #13
  "Only 523-gene panel and WGS",        #14
  "Both panels and WGS"                 #15
), 
group = c(
  "dropout",                             #0
  "stage",                               #1
  "dropout",                             #2
  "stage",                               #3
  "fail",                                #4
  "fail",                                #5
  "fail",                                #6
  "fail",                                #7
  "stage",                               #8
  "CUP",                                 #9
  "dropout",                             #10
  "dropout",                             #11
  "dropout",                             #12
  "WGS",                                 #13
  "523gene",                             #14
  "50gene"                               #15
)
)

# Links
links <- data.frame(
  source = c(0, 0, 1, 1, 1, 1, 1, 3, 3, 3, 3, 8, 8, 8, 8, 9, 9, 9, 9),
  target = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 12, 13, 14, 15),
  value  = c(888, 164, 793, 38, 39, 14, 3, 600, 123, 58, 12, 169, 19, 115, 297, 34, 3, 39, 47)
)

# Aesthetics
node_colour_format <- "d3.scaleOrdinal()
                        .domain(['dropout', 'stage', 'fail', 'CUP', 'WGS', '523gene', '50gene'])
                        .range(['#e6e6e6', '#a5dbfa', '#cb3535', '#c992e6', '#800080', '#ffd42a', '#c8e9fc'])" 

sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        NodeGroup = "group",
                        colourScale = node_colour_format,
                        sinksRight = FALSE,
                        fontSize = 0,
                        fontFamily = "Helvetica",
                        nodeWidth = 30,
                        nodePadding = 28,
                        width = 1000,
                        height = 500)

# Save
saveWidget(
  sankey,
  file = "output/Figure1_feasibility_sankey_raw.html",
  selfcontained = TRUE
)
