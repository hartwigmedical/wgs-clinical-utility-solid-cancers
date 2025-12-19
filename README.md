# Clinical utility of WGS for solid cancers
Study by NKI-AVL on the real-world clinical utility of whole genome sequencing (WGS) for routine diagnostics in solid cancers

## Main Figures

### Figure 1 – Feasibility and panel coverage (Sankey diagram)
- Script: `code/figure1_sankey.R`
- Environment: R 4.4.2, RStudio 2024.09.1+394
- Required packages (tested): `networkD3` (v0.4), `htmlwidgets` (v1.6.4), `extrafont` (v0.20)
- Input: node and link definitions are hard-coded in the script and correspond to the curated WGS cohort described in the manuscript.
- Output: running the script creates `output/Figure1_sankey_raw.html`.

The HTML file can be opened in a web browser and exported to SVG.  
Final node positions and labels were adjusted manually in Inkscape before exporting the high-resolution image submitted for publication.
The underlying node and link values are provided in the Source Data file `SourceData_Main+ED.xlsx`, sheet `Fig1`.

### Figure 2 - 
