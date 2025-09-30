# VR_ICU — Code & materials for *Immersive Virtual Reality Use in Medical Intensive Care: Mixed Methods Feasibility Study*

> Statistical processing and diagramming for the University of Utah immersive VR feasibility study in the ICU (JMIR Serious Games, 2024).

---

## Links & identifiers

- **Paper (publisher):** https://games.jmir.org/2024/1/e62842/  
- **DOI:** https://doi.org/10.2196/62842  
- **PubMed:** https://pubmed.ncbi.nlm.nih.gov/39046869/  
- **PMCID (open access):** PMC11344185 — https://pmc.ncbi.nlm.nih.gov/articles/PMC11344185/  
- **This repository:** https://github.com/reblocke/vr_icu  

---

## How to cite

If you use this code or reproduce the results, please cite the paper:

> **Locke BW**, **Tsai T‑y**, **Reategui‑Rivera CM**, **Gabriel AS**, **Smiley A**, **Finkelstein J**. *Immersive Virtual Reality Use in Medical Intensive Care: Mixed Methods Feasibility Study.* **JMIR Serious Games**. 2024;12:e62842. https://doi.org/10.2196/62842

You may also cite the software (this repository) by referencing the commit hash above or a release DOI (if archived).

---

## What’s here

- `VR_ICU.do` — Stata 18 script for data cleaning, descriptive statistics, and pre–post comparisons reported in the paper (mood, anxiety, pain; heart rate and HRV summaries).  
- `VR Consort Diagram.qmd` — Quarto document that generates the CONSORT‑style enrollment flow diagram used in the manuscript.  
- `LICENSE` — MIT License (see **License** below).

> **Data are not included** in this repository due to human‑subjects protections. See **Data access** for how to request access.

---

## Quick start — reproduce core results

> These steps assume you have an IRB‑approved copy of the de‑identified dataset described in the paper. See **Data access** below.

### 1) Install prerequisites
- **Stata 18** (SE/MP recommended).  
- **Quarto** (to render the `.qmd` diagram): https://quarto.org

### 2) Arrange files
Create the following layout locally (adjust paths as needed):
```
vr_icu/
├── data/                   # place IRB‑approved, de‑identified analysis dataset here
├── outputs/                # figures and tables will be written here
├── VR_ICU.do
├── VR Consort Diagram.qmd
└── LICENSE
```

### 3) Run the analyses (Stata)
From a terminal, run Stata in batch mode (choose the executable that matches your platform):

**macOS/Linux**
```bash
stata -b do VR_ICU.do
# or: stata-se -b do VR_ICU.do
# or: stata-mp -b do VR_ICU.do
```

**Windows (PowerShell)**
```powershell
& "C:\Program Files\Stata18\StataSE-64.exe" /e do VR_ICU.do
# or: wstata /e do VR_ICU.do
```

The script will read the analysis dataset, perform the pre–post comparisons, and save outputs (tables/figures) under `outputs/` (paths can be edited at the top of the `.do` file if needed).

### 4) Build the enrollment flow diagram (Quarto)
```bash
quarto render "VR Consort Diagram.qmd"
```
The rendered file (HTML/PDF/PNG depending on your Quarto settings) will appear alongside the source.

---

## Data access

Per the paper’s **Data Availability** statement, de‑identified data can be provided **upon IRB‑approved request**. Please contact **Joseph Finkelstein, MD, PhD** (University of Utah) at `Joseph.Finkelstein@utah.edu` to initiate a request. Place the approved dataset under `data/` as shown above.

> The analysis assumes that HRV features were precomputed from raw ECG using **Kubios HRV**; the exported metrics are then analyzed in Stata. If you are preparing a new dataset, include the HR and HRV summaries used in the manuscript.

---

## Methods notes

- **Setting & design.** Single‑center, prospective mixed‑methods feasibility study in two ICUs (25‑bed medical ICU and 16‑bed cancer‑specialty ICU).  
- **Device & monitoring.** **Meta Quest Pro** headset; **BIOPAC MP160** used for 5‑minute pre‑ and post‑VR physiological recordings.  
- **VR content.** Patients chose one of: **YouTube VR** (urban travel), **Nature Treks VR** (nature scenes), or **TRIPP** (synthetic landscapes); passive exploration without controllers.  
- **Session length.** Planned 5–15 minutes of VR exposure.  
- **Participants.** 35 approached; **20 (57%)** participated; **19 completed** the VR session.  
- **Outcomes (pre→post).** Improvements in overall mood (Δ≈+1.8/10), anxiety (Δ≈‑1.7/10), and pain (Δ≈‑1.3/10); mean heart rate decreased ≈1.1 bpm; HRV stress index decreased ≈5 s⁻²; **no adverse events or cybersickness** observed.

For full details and exact statistics, see the open‑access article linked above.

---

## Repository layout

```
.
├── VR_ICU.do                   # Stata 18 analysis
├── VR Consort Diagram.qmd      # Quarto source for enrollment flow diagram
├── LICENSE                     # MIT license
└── (optional) docs/            # poster & slide deck (see below)
```

---

## Results mapping (paper ↔ code)

| Paper item | Where in this repo | How to regenerate |
|---|---|---|
| **Figure 3. Enrollment flow diagram** | `VR Consort Diagram.qmd` | `quarto render "VR Consort Diagram.qmd"` |
| **Figure 4. Mood, anxiety, pain (pre–post)** | `VR_ICU.do` | Run Stata in batch: `stata -b do VR_ICU.do` |
| **Table 2. Participants vs decliners** | `VR_ICU.do` | Run Stata as above; exports table under `outputs/` |
| **Figures 1–2. Setup & screenshots** | not generated by code | Provided in the manuscript; illustrative only |

---

## Funding & acknowledgments

This work was supported in part by the **National Institutes of Health**: **R33 HL143317 (NHLBI)**, **T32 HL105321 (NHLBI)**, and **UM1 TR004409 (NCATS)**. The content is solely the responsibility of the authors and does not necessarily represent the official views of the NIH.

We thank the ICU nursing and clinical teams for enabling bedside data collection and patient participation.

---

## License

This repository is released under the **MIT License** (see `LICENSE`).

- **Code:** MIT.  
- **Manuscript & figures:** follow the publisher’s license (**CC BY 4.0**) when reusing article content; credit the original paper and license.

---

## Maintainer & contact

- **Maintainer:** Brian W. Locke, MD, MSc — `brian.locke@imail.org`  
- **Issues:** please open a GitHub issue with a minimal reproducible example and your environment details (OS, Stata version, Quarto version).