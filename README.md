# A New Analysis of the Shark and Ray Fossil Record Reveals Hidden Diversity Patterns

This repository contains the code and data for the analyses presented in the manuscript:

**"The hidden patterns of neoselachian diversity over the past 145 million years"**  
**A. Gardiner1, G. H. Mathes1, R. Cooper2,3, K. Kocáková1, J.A. Villafaña4, D. Silvestro2,3,5*, C. Pimiento1,6\***  

¹ Department of Paleontology, University of Zurich, Zurich, 8006, Switzerland  
² Department of Biology, University of Fribourg, Fribourg, CH-1700, Switzerland  
³ Swiss Institute of Bioinformatics, Lausanne, 1015, Switzerland  
⁴ Departamento de Ecología, Facultad de Ciencias, Universidad Católica de la Santísima Concepción, Concepción 4090541, Chile  
⁶ Department of Biological and Environmental Sciences, University of Gothenburg, Gothenburg, 405 30, Sweden  
⁷ Department of Biosciences, Swansea University, Swansea, SA2 8PP, United Kingdom  

\*Contributed equally and are the corresponding authors: [daniele.silvestro@unifr.ch](mailto:daniele.silvestro@unifr.ch); [catalina.pimientohernandez@pim.uzh.ch](mailto:catalina.pimientohernandez@pim.uzh.ch)

---

## Overview

This repository provides the scripts and data used to explore diversity patterns in sharks and rays (supergroup: Neoselachii) using diversity estimates derived from DeepDive. The analyses include visualizations, comparisons across diversity metrics, and assessments of percentage changes in diversity over time.

### Repository Structure

- **`data/`**  
  Contains the diversity estimates from DeepDive and any additional input data files used for the analyses.

- **`R/`**  
  Contains the R scripts for the analyses.

- **`main.figures.R`**  
  Generates plots of DeepDive diversity estimates across supergroups and orders at the species and genus levels for the main figures of the manuscript.

- **`visualise_river_plot.R`**  
  Creates supplementary visualizations, including the fluvial plot of ocean basins over time.

- **`calculate_continuous_diversity.R`**  
  Calculates raw diversity and SQS diversity estimates for comparison with DeepDive results.

- **`visualise_continuous.R`**  
  Visualizes and compares DeepDive diversity estimates with raw, SQS, and PyRate diversity estimates at species and genus levels.

- **`spline_model.R`**  
  Calculates percentage changes in diversity over time across different diversity metrics.

- **`Tables.R`**  
  Creates the tables for the manuscript.


