# Technical Documentation: TaxoConsensus Pro (PINV01-528)

This document provides the technical specifications, implementation details, and deployment guidelines for the **TaxoConsensus Pro** dashboard, developed under project **PINV01-528**.

## 1. Executive Summary

**Project ID:** PINV01-528

**Scope:** Taxonomic Curation, Epidemiological Surveillance, and Vector Data Standardization.

**Core Objective:** To bridge the gap between raw field entomological data and standardized scientific nomenclature for high-impact research publications.

## 2. Technical Implementation

The dashboard is built as a high-performance **Single Page Application (SPA)**, ensuring zero-latency transitions and a self-contained architecture suitable for offline archiving.

### System Architecture

-   **Frontend Framework:** Tailwind CSS (Responsive Design).

-   **Data Visualization:** Chart.js (Real-time mapping confidence analytics).

-   **AI Integration:** Google Gemini API (Model: `gemini-2.5-flash-preview-09-2025`).

-   **Language Support:** Multi-language dictionary (English, Spanish, Portuguese).

### Data Integrity Protocols

To ensure absolute scientific accuracy and comply with peer-review standards:

1.  **Hallucination Prevention:** The system avoids automated generation of LSIDs or DOIs.

2.  **Manual Auditing:** Each record is linked directly to the **Global Biodiversity Information Facility (GBIF)** search engine for expert verification.

3.  **Standardization:** Mapping is aligned with **Darwin Core (DwC)** terms.

## 3. Artificial Intelligence Modules

The Gemini API is utilized for two primary scientific tasks:

### A. Taxonomic Status Resolution

Provides expert-level justifications for nomenclatural changes (e.g., *Lutzomyia* to *Nyssomyia*), citing authors and years while maintaining a strict focus on clinical and entomological evidence.

### B. Global Epidemiological Synthesis

An aggregator function that processes the entire dataset of 22 species to generate a country-level risk report, identifying potential hotspots and transmission threats in Paraguay.

## 4. Deployment and Open Science

### Interactive Hosting (GitHub Pages)

The dashboard is designed for immediate deployment as an interactive supplemental material.

1.  Upload `index.html` to a public repository.

2.  Enable **GitHub Pages** via settings.

3.  Include the resulting URL in the manuscript's data availability section.

### Permanent Archiving (Zenodo)

For long-term reproducibility:

-   **Identifier:** Persistent DOI assignment.

-   **Metadata:** All software releases must be linked to project **PINV01-528**.

-   **Licensing:** Recommended under **Creative Commons Attribution 4.0 International**.

## 5. Master Prompt for Reproduction

The following logic is used to replicate the system's behavior:

> "Act as a Senior Data Scientist. Create a Single Page Application titled 'TaxoConsensus Pro' for project PINV01-528. Integrate a dataset of 22 vector species with GBIF-matched names. Implement a logic to call the Gemini API for taxonomic resolution (Correct Name, Justification, Medical Importance). Avoid hallucinating IDs; provide direct GBIF links instead. Include a Global Report feature for epidemiological synthesis."

*Document generated for the PINV01-528 Curation Pipeline: R-Tidy + Gemini AI Hub.*
