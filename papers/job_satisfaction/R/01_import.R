# Job satisfaction paper: raw import, Korea linkage, coverage audit.
# Run from the repository root: Rscript papers/job_satisfaction/R/01_import.R
options(warn = 1)
library(haven)
base <- "papers/job_satisfaction"
out <- file.path(base, "results")
dir.create(out, showWarnings = FALSE)

files <- list.files("01_data", "^PRG[A-Z]{3}P2.sav$", recursive = TRUE, full.names = TRUE)
traits <- c("EXTR", "CONS", "OPEM", "EMOS", "AGRE")
traits6 <- paste0(traits, "_6")
character_vars <- c("SEQID", "VEMETHOD", "ISCO1C", "ISCO2C", "ISCO08_C", "ISIC1C")
facets <- c("EXTR_SOCI", "EXTR_ASSE", "EXTR_ENER", "AGRE_COMP", "AGRE_RESP", "AGRE_TRUS", "CONS_ORGA", "CONS_PROD",
  "CONS_RESP", "EMOS_ANXI", "EMOS_DEPR", "EMOS_VOLA", "OPEM_AEST", "OPEM_CURI", "OPEM_IMAG")
job_items <- c("H2_Q12", "H2_Q17", "H2_Q07a", paste0("H2_Q18a0", 1:7), paste0("H2_Q08", letters[1:4]), paste0("H2_Q09", letters[1:3]),
  paste0("H2_Q03", letters[1:4]), paste0("H2_Q05", letters[1:2]), paste0("H2_Q13", letters[1:3]), paste0("H2_Q14", letters[1:2]))
vars <- c("SEQID", "C2_D05", "D2_Q04", "AGEG10LFS", "AGE_R", "GENDER_R", "EDCAT6_TC1",
  "EDA_ISCED1", "EDA_ISCED1_C", "A2_Q03a", "J2_Q04c", "J2_Q05c",
  "D2_Q13", "I2_Q05", "I2_Q03", "H2_Q19a", traits, traits6, facets, job_items, "ISIC1C",
  "ISCO1C", "ISCO2C", "ISCO08_C", "ISCOSKIL4", "D2_Q03", "D2_Q05a2", "D2_Q07a", "D2_Q09a",
  "D2_Q10_C", "D2_Q11_C", "TASKDISCC2_T1", "LEARNATWORKC2", "EARNHRBONUSC2", "EARNHRBONUSDCLC2",
  paste0("PVNUM", 1:10), paste0("PVLIT", 1:10), paste0("PVAPS", 1:10),
  paste0("SPFWT", 0:80), "VEMETHOD", "VENREPS", "VEFAYFAC")

dat <- lapply(files, function(f) {
  d <- as.data.frame(read_sav(f, col_select = tidyselect::any_of(vars)))
  for (v in setdiff(vars, names(d))) d[[v]] <- NA
  for (v in setdiff(vars, character_vars)) d[[v]] <- as.numeric(d[[v]])
  for (v in character_vars) d[[v]] <- trimws(as.character(d[[v]]))
  # Keep the employed only; nothing in this paper uses anyone else.
  d[d$C2_D05 %in% 1, vars, drop = FALSE]
})
names(dat) <- substr(basename(files), 4, 6)

# Korea distributes personality separately. Match respondent keys, never row order.
kfile <- "01_data/prgkorp2_sav/prg_2korms_big5.sav"
k <- as.data.frame(read_sav(kfile, col_select = tidyselect::all_of(c("SEQID", traits, traits6, facets))))
k$SEQID <- trimws(as.character(k$SEQID))
k <- k[!is.na(k$SEQID) & k$SEQID != "", ]
stopifnot(!anyDuplicated(k$SEQID), !anyDuplicated(dat$KOR$SEQID))
ix <- match(dat$KOR$SEQID, k$SEQID)
cat("Korea: employed respondents matched to the personality file:", sum(!is.na(ix)), "of", length(ix), "\n")
for (v in c(traits, traits6, facets)) dat$KOR[[v]] <- as.numeric(k[[v]][ix])
saveRDS(dat, file.path(out, "raw_selected.rds"))

# Coverage among employees 25-65 with a satisfaction answer (non-missing values, not column presence).
audit <- do.call(rbind, lapply(names(dat), function(cc) {
  d <- dat[[cc]]
  e <- d$D2_Q04 %in% 1 & d$AGEG10LFS %in% 2:5 & is.finite(d$SPFWT0) & d$SPFWT0 > 0
  s <- e & d$D2_Q13 %in% 1:5
  share <- function(v) round(mean(is.finite(d[[v]][s])), 3)
  data.frame(country = cc, employed = nrow(d), employees_25_65 = sum(e), satisfaction = sum(s),
    big5 = sum(s & complete.cases(d[traits])), big5_6 = sum(s & complete.cases(d[traits6])),
    numeracy = sum(s & complete.cases(d[paste0("PVNUM", 1:10)])),
    wage = sum(s & is.finite(d$EARNHRBONUSC2) & d$EARNHRBONUSC2 > 0),
    hours = share("D2_Q11_C"), contract = share("D2_Q10_C"), tenure = share("D2_Q05a2"),
    taskdisc = share("TASKDISCC2_T1"), underuse = round(mean(d$H2_Q19a[s] %in% 1:3), 3),
    life_sat = round(mean(d$I2_Q05[s] %in% 0:10), 3),
    method = paste(unique(d$VEMETHOD), collapse = ";"), reps = paste(unique(d$VENREPS), collapse = ";"))
}))
write.csv(audit, file.path(out, "coverage.csv"), row.names = FALSE)
src <- c(files, kfile)
write.csv(data.frame(path = src, md5 = unname(tools::md5sum(src))), file.path(out, "inputs.csv"), row.names = FALSE)
print(audit, row.names = FALSE)
