# Unpack the OECD PIAAC Cycle 2 public-use archives in 01_data/ into the layout the
# import expects: PRG<CNT>P2.sav in 01_data/, and the Korean main and personality files
# in 01_data/prgkorp2_sav/. Run from the repository root:
#   Rscript papers/job_satisfaction/R/00_unpack_data.R
zips <- list.files("01_data", "^prg[a-z]{3}p2_sav\\.zip$", full.names = TRUE)
stopifnot(length(zips) > 0)
for (z in zips) {
  dest <- if (grepl("prgkorp2", z)) "01_data/prgkorp2_sav" else "01_data"
  dir.create(dest, showWarnings = FALSE)
  utils::unzip(z, exdir = dest, overwrite = FALSE)
}
cat("Unpacked", length(zips), "archives\n")
