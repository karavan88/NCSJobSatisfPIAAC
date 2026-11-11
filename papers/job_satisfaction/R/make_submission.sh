#!/bin/sh
# Submission package for Social Science Quarterly. Run from the repository root.
# Word files: title page, anonymised manuscript, Appendix. PDF: Appendix.
set -e
export OMP_NUM_THREADS=1
cd papers/job_satisfaction/manuscript
S=../output/submission; mkdir -p "$S"
for f in title_page appendix job_satisfaction; do
  quarto render $f.qmd --to docx --output-dir "$S"
done
quarto render appendix.qmd --to pdf --output-dir "$S"
mv "$S/appendix.pdf" "$S/tmp.pdf" && mv "$S/tmp.pdf" "$S/Appendix.pdf"  # case-insensitive file system
python3 ../R/fix_docx.py "$S/title_page.docx" "$S/title_page.docx"
python3 ../R/fix_docx.py "$S/appendix.docx" "$S/tmp.docx" --appendix && rm "$S/appendix.docx" && mv "$S/tmp.docx" "$S/Appendix.docx"
python3 ../R/fix_docx.py "$S/job_satisfaction.docx" "$S/manuscript_anonymised.docx" && rm "$S/job_satisfaction.docx"
ls -la "$S"
