# Raw data

The analysis uses 28 archives with the public-use files of the second cycle of the
OECD Programme for the International Assessment of Adult Competencies (PIAAC,
Survey of Adult Skills 2023), in SPSS format, as distributed by the OECD. They are not included in this repository. Download them from
<https://www.oecd.org/skills/piaac/data/> and place the zip files (`prgautp2_sav.zip`, `prgbelp2_sav.zip`, …, `prgusap2_sav.zip`) in this folder.

`prgkorp2_sav.zip` contains the Korean main file and the separately released Korean
Big Five file (`prg_2korms_big5.sav`), which the import links by respondent ID.

Unpack them with `make data` (or `Rscript papers/job_satisfaction/R/00_unpack_data.R`).
`papers/job_satisfaction/results/inputs.csv` lists the MD5 checksum of every unpacked
file used in the analysis.

Source: OECD (2024). *Survey of Adult Skills 2023: Public Use Files*. OECD Publishing.
Use of the data is subject to the OECD's terms and conditions.
