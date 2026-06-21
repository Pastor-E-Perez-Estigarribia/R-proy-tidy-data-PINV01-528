# Data Processing and Tidy Data Workflow - Project PINV01-528

This repository contains the end-to-end workflow for data processing, cleaning, and structuring for **Project PINV01-528**. The primary goal is to transform raw SENEPA datasets into a "tidy" format optimized for statistical analysis and mathematical modeling.

## 🚀 Live Interactive Report
The detailed analysis, quality control results, and tidy datasets can be accessed via the project's web interface:
👉 **[View PINV01-528 Project Report](https://pastor-e-perez-estigarribia.github.io/R-proy-tidy-data-PINV01-528/)**

## TaxoConsensus Hub
**[TaxoConsensus Hub](https://pastor-e-perez-estigarribia.github.io/R-proy-tidy-data-PINV01-528/TaxoConsensus/technical_report_manual.html)** is an interactive web application developed in R Shiny. Its primary purpose is to assist researchers and epidemiologists in resolving the taxonomic status of medically important vectors and evaluating epidemiological risk.

The tool natively integrates generative artificial intelligence (Google Gemini) into the R environment, combining the rigor of classic data analysis with Natural Language Processing (NLP) capabilities, ensuring compliance with strict data integrity protocols

## 📊 Repository Structure
* **`index.Rmd`**: Main R Markdown script generating the final interactive report.
* **`Scripts/`**: R helper functions for specialized data cleaning.
* **`data/`**: (Internal) Source and processed datasets.
* **`reports/`**: Quality Assurance (QA) documents and previous iterations.
* **`SENEPA_tidy_data_set.csv`**: The final tidied output ready for modeling.

## 🛠️ Tech Stack
* **Language:** R 4.x
* **Core Libraries:** `tidyverse` (dplyr, tidyr, ggplot2), `pacman`, `rmarkdown`.
* **Version Control:** Git & GitHub.
* **Environment:** RStudio.

---
**Affiliation:** Facultad Politécnica - UNA / Project PINV01-528
