# Objetivo: Generar y exportar el diagrama de flujo metodológico para el Data Paper
# Requiere instalación previa de: install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))

if (!require('pacman'))
  install.packages('pacman')
pacman::p_load(DiagrammeR, DiagrammeRsvg, rsvg)

# 1. Definir la estructura del gráfico usando sintaxis Graphviz (dot)
flujo_datos <- grViz(
  "
digraph workflow {
  # Configuración global del gráfico
  graph [layout = dot, rankdir = TB, compound = true, fontname = 'Helvetica', fontsize = 12]

  # Estilo global de los nodos (cajas)
  node [shape = box, style = 'filled,rounded', fontname = 'Helvetica',
        fillcolor = '#e1f5fe', color = '#0288d1', penwidth = 2]

  # Estilo global de las flechas
  edge [color = '#666666', penwidth = 1.5]

  # Fase 1: Preparación
  subgraph cluster_prep {
    label = 'Phase 1: Preparation';
    style = 'rounded,dashed'; color = '#999999';
    A [label = 'Raw Data Files\\n(Excel / CSV)', fillcolor = '#f9f9f9', color = '#333333'];
    B [label = 'Segmentation by Vector Group\\n(Phlebotominae, Triatominae...)'];
  }

  # Fase 2: Limpieza y Normalización
  subgraph cluster_clean {
    label = 'Phase 2: Cleaning & Normalization';
    style = 'rounded,dashed'; color = '#999999';
    C [label = 'Header Standardization\\n(ASCII, Uppercase)'];
    D [label = 'Text Normalization\\n(iconv, str_trim)'];
    E [label = 'Missing Value Handling\\n(Replace S/D, 0 with NA)'];
    F [label = 'Date Imputation\\n(DATE_aux = Jan 1st)'];
  }

  # Fase 3: Corrección Taxonómica con IA
  subgraph cluster_ai {
    label = 'Phase 3: AI Taxonomic Correction';
    style = 'rounded,dashed'; color = '#8e24aa';
    node [fillcolor = '#f3e5f5', color = '#8e24aa'];
    G [label = 'SPECIES_RAW Extraction'];
    H [label = 'AI Agent (Gemini)', shape = 'diamond', fillcolor = '#ce93d8'];
    I [label = 'Generation of SPECIES_AI_Correct\\n& Taxonomic Metadata'];
  }

  # Fase 4: Control de Calidad y Consolidación
  subgraph cluster_qc {
    label = 'Phase 4: Quality Control & Consolidation';
    style = 'rounded,dashed'; color = '#f57c00';
    node [fillcolor = '#fff3e0', color = '#f57c00'];
    J [label = 'Dataset Consolidation\\n(bind_rows)'];
    K [label = 'Addition of Provenance Metadata\\n(DATA_SOURCE)'];
    L [label = 'QA Report Generation\\n(summarytools, data_dict_md)'];
  }

  # Fase 5: Exportación
  subgraph cluster_export {
    label = 'Phase 5: Export';
    style = 'rounded,dashed'; color = '#388e3c';
    M [label = 'SENEPA_tidy_data_set.csv', fillcolor = '#e8f5e9', color = '#388e3c', shape = 'cylinder'];
  }

  # Conexiones entre los nodos
  A -> B
  B -> C
  C -> D -> E -> F
  F -> G
  G -> H
  H -> I [label = ' Validation &\\nCorrection', fontsize = 10, fontcolor = '#8e24aa']
  I -> J
  J -> K -> L
  L -> M
}
")

# 2. Visualizar el gráfico en el Viewer de RStudio
print(flujo_datos)

# 3. Exportar el gráfico a PNG de alta resolución (300 DPI ideal para Data Papers)
# Descomenta las siguientes líneas cuando estés listo para exportar:

# flujo_datos %>%
#   export_svg() %>%
#   charToRaw() %>%
#   rsvg_png("output/fig/data_workflow.png", width = 1200)

# También puedes exportarlo en formato vectorial puro (SVG o PDF):
flujo_datos %>% export_svg() %>% charToRaw() %>% rsvg_pdf("output/fig/data_workflow.pdf")