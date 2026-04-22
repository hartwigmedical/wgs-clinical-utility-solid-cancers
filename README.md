# WGS clinical utility in solid cancers
This repository contains the figure-generation code for the manuscript on the real-world clinical utility of routine tumor whole-genome sequencing (WGS) in solid cancers.

## Repository purpose
The scripts in this repository generate the main figures, supplementary figures, and extended data figures from the curated source data file: `data/SourceData_Main+ED.xlsx`

This repository is intended to document figure generation from the final curated source data that accompany the manuscript. It does not contain the full history of intermediate counting steps.

## Publication

**Manuscript title:** Real-world clinical utility of tumor whole-genome sequencing in solid cancers
**Journal:** Nature Medicine  
**Status:** Published online on 20 March 2026
**DOI:** 10.1038/s41591-026-04280-2

## Software environment
The scripts were developed and run in:
- R 4.4.2
- RStudio 2024.09.1+394

Required packages vary slightly by script, but mainly include:
- readxl
- dplyr
- tidyr
- ggplot2
- survival
- survminer
- patchwork

## Input data
By default, the figure scripts read sheets from:
- `data/SourceData_Main+ED.xlsx`

This workbook contains the curated source data used for figure generation. It is not equivalent to raw clinical data or raw electronic health record exports.

## Repository structure
The repository currently contains separate R scripts for each figure or figure panel.

### Main figures
- `figure1_feasibility_sankey.R`
- `figure2a_actionability_boxplots.R`
- `figure2b_actionability_counts.R`
- `figure2c_tmb_density.R`
- `figure2d_actionability_tumor_types.R`
- `figure3_cup_summary.R`
- `figure4ab_clinical_utility.R`
- `figure5a_os_total.R`
- `figure5b_os_actionable_only.R`
- `figure5c_os_pretreatments_split.R`

### Supplementary figures
- `supp_fig1_actionability_grid.R`
- `supp_fig2a_os_non_bit.R`
- `supp_fig2b_os_pretreatments_non_bit.R`
- `supp_fig2c_os_pretreatments_bit.R`
- `supp_fig2d_os_known_vs_cup.R`

### Extended data figures
- `ed1_tat_histogram.R`
- `ed3_vignette_cohort_bars.R`
- `ed5_germline_grid.R`

## Output
Scripts write figure files to: `output/`
Most scripts export .pdf files.

## Notes on figure assembly
Where applicable, figures were generated in R and then assembled into the final journal figure layout in Inkscape.

This includes tasks such as:
- combining separately generated panels
- aligning panel positions
- placing panel letters
- adjusting label positions for legibility

No underlying data values were altered during final figure assembly.
For figure-specific details, see the header comments in each script.

## Reproducibility notes
- Scripts are designed to run from the repository root.
- Input paths are set relative to the repository structure.
- Some figures were assembled from multiple exported panels.
- In a small number of cases, labels were generated separately and combined during final assembly in Inkscape.
- Some manuscript counts were derived from curated tables and figure-specific source sheets rather than from a single unified counting script.

## Data availability
This repository contains code and curated source data used to generate the published figures.
It does **not** contain:
- raw clinical data
- raw electronic health record exports
- direct patient identifiers
- key files used for re-identification

Requests for access to non-public underlying data should follow the data access and governance procedures described in the manuscript.

## Contact
For questions about the analysis code or figure generation, please contact the corresponding authors listed in the manuscript: E. Cuppen or K. Monkhorst.
