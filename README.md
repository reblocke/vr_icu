# VR ICU: Analysis Code for Immersive Virtual Reality Use in Medical Intensive Care

[![DOI](https://img.shields.io/badge/DOI-10.2196%2F62842-blue)](https://doi.org/10.2196/62842)
[![PMID](https://img.shields.io/badge/PMID-39046869-blue)](https://pubmed.ncbi.nlm.nih.gov/39046869/)
[![PMCID](https://img.shields.io/badge/PMCID-PMC11344185-blue)](https://pmc.ncbi.nlm.nih.gov/articles/PMC11344185/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Statistical processing and diagramming code for **"Immersive Virtual Reality Use in Medical Intensive Care: Mixed Methods Feasibility Study"**, published in *JMIR Serious Games* in 2024.

## Links and Identifiers

- Final article: <https://games.jmir.org/2024/1/e62842/>
- DOI: <https://doi.org/10.2196/62842>
- PubMed: <https://pubmed.ncbi.nlm.nih.gov/39046869/>
- NLM/PMC full text: <https://pmc.ncbi.nlm.nih.gov/articles/PMC11344185/>
- JMIR preprint record: <https://preprints.jmir.org/preprint/62842>
- Repository: <https://github.com/reblocke/vr_icu>
- Machine-readable citation metadata: [CITATION.cff](CITATION.cff)
- Machine-readable repository index: [llms.txt](llms.txt)

## Description

This repository contains the Stata and Quarto code used to support a single-center, prospective mixed-methods feasibility study of immersive virtual reality in critically ill adults. The workflow cleans restricted study workbooks, summarizes participation and participant characteristics, analyzes pre-post patient-reported mood, anxiety, and pain scores, analyzes heart rate and heart rate variability summaries, and renders the enrollment flow diagram.

The raw and deidentified participant-level data are not committed because access requires Institutional Review Board approval. The final article, preprint record, and NLM version are linked above; manuscript text is not mirrored in this repository.

## Quick Start

These steps assume you have an IRB-approved local copy of the restricted study data.

### Requirements

| Component | Required version or source | Purpose |
| --- | --- | --- |
| Stata | 18 IC, SE, or MP | Main analysis workflow |
| `table1_mc` | SSC | Descriptive tables |
| `catplot` | SSC | Categorical plots |
| `cleanplots` | `net install cleanplots, from("https://tdmize.github.io/data")` | Stata plot scheme |
| Quarto | Current local install | Enrollment diagram rendering |
| R packages | `consort`, `dplyr`, `haven`, `rsvg`, `DiagrammeRsvg`, `magick` | Quarto diagram workflow |
| Kubios HRV | External preprocessing | Heart rate variability features analyzed in Stata |

Install Stata packages once:

```stata
ssc install table1_mc
ssc install catplot
net install cleanplots, from("https://tdmize.github.io/data")
```

Install R packages once:

```r
install.packages(c("consort", "dplyr", "haven", "rsvg", "DiagrammeRsvg", "magick"))
```

### Arrange Local Files

Place the approved restricted inputs under `data/`:

```text
vr_icu/
|-- data/
|   |-- ICU_patient_database.xlsx
|   |-- ICU_selection_database.xlsx
|   `-- hr_data.xlsx
|-- outputs/
|-- VR_ICU.do
`-- VR Consort Diagram.qmd
```

### Run the Stata Analysis

Run from the repository root:

```bash
stata-mp -b do VR_ICU.do
```

The do-file defaults to `data/` for inputs and `outputs/stata/` for generated files. You can pass explicit directories:

```bash
stata-mp -b do VR_ICU.do data outputs/stata
```

The Stata workflow fails early with a clear message if required restricted inputs are absent.

### Render the Enrollment Flow Diagram

After the Stata workflow creates `outputs/stata/db_for_consort.dta`, run:

```bash
quarto render "VR Consort Diagram.qmd"
```

The Quarto workflow writes `consort.png` and `consort_dpi300.png` under `outputs/figures/`. To override paths, set `VR_ICU_CONSORT_DTA` or `VR_ICU_FIGURE_DIR`.

## Data Access and Codebook

Per the article's data availability statement, deidentified data can be provided upon Institutional Review Board-approved request to Joseph Finkelstein, MD, PhD, at `Joseph.Finkelstein@utah.edu`.

Variable-level documentation is provided in:

- Human-readable codebook: [data_dictionary.md](data_dictionary.md)
- Machine-usable CSV codebook: [data_dictionary.csv](data_dictionary.csv)

The analysis assumes heart rate variability features were precomputed from raw ECG using Kubios HRV and then exported for Stata analysis.

## File Inventory

| Path | Type | Description |
| --- | --- | --- |
| `VR_ICU.do` | Stata script | Main data cleaning, descriptive tables, pre-post analyses, and figure export workflow |
| `VR Consort Diagram.qmd` | Quarto document | Enrollment flow diagram rendering from derived Stata data |
| `data_dictionary.md` | Documentation | Human-readable documentation for expected inputs and derived variables |
| `data_dictionary.csv` | Documentation | Machine-usable data dictionary |
| `CITATION.cff` | Citation metadata | Citation metadata for the article and repository |
| `llms.txt` | Machine-readable index | Compact repository orientation for machine readers |
| `AGENTS.md` | Agent instructions | Repository-specific instructions for coding agents |
| `LICENSE` | License | MIT license for repository code |
| `.gitignore` | Hygiene | Prevents restricted data and generated outputs from being committed |

## Workflow and Outputs

The Stata script writes dated output directories under `outputs/stata/<Stata date>/` and logs under `outputs/stata/<Stata date>/Logs/`. It also writes `outputs/stata/db_for_consort.dta`, which is the input for the Quarto flow diagram.

Expected outputs include:

| Output | Source | Role |
| --- | --- | --- |
| `All patients Demographics.xlsx` | `VR_ICU.do` | Participant and decliner demographics |
| `All patients comorbs.xlsx` | `VR_ICU.do` | Comorbidity summaries |
| `All patients VR chars.xlsx` | `VR_ICU.do` | VR exposure and selection summaries |
| `All patients data quality.xlsx` | `VR_ICU.do` | Pre-post physiologic data quality summaries |
| `VAS q1-3 combined.png` | `VR_ICU.do` | Mood, anxiety, and pain pre-post display |
| `HR pre-post indiv.png` | `VR_ICU.do` | Individual heart rate pre-post display |
| `HRV pre-post indiv.png` | `VR_ICU.do` | Individual HRV stress index pre-post display |
| `consort_dpi300.png` | `VR Consort Diagram.qmd` | Enrollment flow diagram |

## Results Mapping

| Paper item | Where in this repo | How to regenerate |
| --- | --- | --- |
| Figure 3, enrollment flow diagram | `VR Consort Diagram.qmd` | Run Stata, then `quarto render "VR Consort Diagram.qmd"` |
| Figure 4, mood/anxiety/pain pre-post display | `VR_ICU.do` | `stata-mp -b do VR_ICU.do` |
| Table 2, participants vs decliners | `VR_ICU.do` | `stata-mp -b do VR_ICU.do` |
| Figures 1-2, setup and screenshots | Article only | Illustrative manuscript media, not generated here |

## Authors, Funding, and Conflicts

Article authors: Brian W. Locke; Te-Yi Tsai; C. Mahony Reategui-Rivera; Aileen S. Gabriel; Aref Smiley; Joseph Finkelstein.

Funding reported in the article includes NIH support through T32HL105321 and R33HL143317. Conflict-of-interest details are reported in the article; repository users should cite the final publication for the authoritative statement.

## Citation

Please cite the final article when using this repository:

> Locke BW, Tsai T, Reategui-Rivera CM, Gabriel AS, Smiley A, Finkelstein J. Immersive Virtual Reality Use in Medical Intensive Care: Mixed Methods Feasibility Study. *JMIR Serious Games*. 2024;12:e62842. doi:10.2196/62842.

Use [CITATION.cff](CITATION.cff) for structured citation metadata and include the repository URL plus the commit or release used when citing the software workflow.

## License and Reuse

- Code: MIT License, see [LICENSE](LICENSE).
- Final article: open-access JMIR article under CC BY 4.0; follow the license terms on the article page and credit the final publication.
- Preprint: linked for provenance only. Do not redistribute or mirror the preprint PDF/text from this repository.
- Restricted data: not licensed for public redistribution and must not be committed.

## Maintenance and Contact

- Maintainer: Brian W. Locke, MD, MSc (`@reblocke`)
- Repository questions: open a GitHub issue with a minimal reproducible example and environment details.
- Data access requests: follow the article's data availability statement and contact Joseph Finkelstein, MD, PhD, at `Joseph.Finkelstein@utah.edu`.

## Machine Readability

This repository uses the following public orientation files:

- `README.md` for human-facing context, run path, data access, and citation.
- `llms.txt` for compact machine-readable repository orientation.
- `AGENTS.md` for coding-agent instructions.
- `CITATION.cff` for structured citation metadata.
- `data_dictionary.md` and `data_dictionary.csv` for expected restricted inputs and derived variables.
