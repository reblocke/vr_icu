# AGENTS

## Project Purpose

This public repository contains Stata and Quarto code for "Immersive Virtual Reality Use in Medical Intensive Care: Mixed Methods Feasibility Study" (JMIR Serious Games, 2024; DOI: 10.2196/62842; PMID: 39046869; PMCID: PMC11344185).

## Public and Data-Safety Rules

- Treat this repository as public research code.
- Do not add PHI, restricted participant-level datasets, credentials, private IRB materials, reviewer correspondence, or publisher-formatted PDFs.
- The expected inputs under `data/` are restricted and are intentionally ignored.
- Do not mirror full article or preprint text in Markdown. Link DOI, PubMed, PMC, JMIR, and preprint records instead.
- Generated Stata and Quarto outputs belong under `outputs/` and should not be committed unless a release explicitly archives them.

## How to Orient Quickly
Consult only the entries relevant to the requested edit or run.

1. Read `README.md` for article links, data access, dependencies, and workflow.
2. Read `llms.txt` for compact machine-readable repository orientation.
3. Use `CITATION.cff` for structured citation metadata.
4. Use `data_dictionary.md` or `data_dictionary.csv` before interpreting expected input or derived variables.
5. Inspect `VR_ICU.do` and `VR Consort Diagram.qmd` before running them.

## Workflow

Default Stata workflow from the repository root:

```bash
stata-mp -b do VR_ICU.do
```

Optional explicit directories:

```bash
stata-mp -b do VR_ICU.do data outputs/stata
```

After Stata creates `outputs/stata/db_for_consort.dta`, render the enrollment diagram:

```bash
quarto render "VR Consort Diagram.qmd"
```

## Verification Before Publishing Changes

- Run `git diff --check`.
- Validate `CITATION.cff` as YAML after citation edits.
- Confirm `README.md`, `llms.txt`, `AGENTS.md`, and `CITATION.cff` agree on DOI, PMID, PMCID, run paths, and data restrictions.
- Search for hard-coded local absolute paths from personal workstations.
- Confirm `.gitignore` catches restricted workbooks, derived `.dta` files, logs, generated figures/tables, and local OS artifacts.
- For analysis/runner changes, perform applicable Stata verification within the authorized workflow. It requires a licensed runtime and the approved inputs; executable availability alone does not authorize a restricted-data run. Inspect generated logs and report unavailable data/package/runtime gates separately from static checks.
- When the diagram or its inputs change, render it within the authorized data scope if Quarto dependencies and approved inputs are available; otherwise report the missing prerequisite.
