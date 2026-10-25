"""Post-process the Word files produced by Quarto for the submission package.

Usage: python3 fix_docx.py in.docx out.docx [--appendix]

1. Quarto writes two <w:pPr> blocks in figure/table caption paragraphs inside
   cross-reference floats; Word expects one. The first (centring only) is dropped.
2. With --appendix, Quarto's own running numbers are removed from captions
   ("Table 1: Table A1. ..." -> "Table A1. ..."), so the appendix keeps the
   A-numbering used in the main text.
3. Captions follow the Springer style: a bold label ("Fig. 1", "Table A3"),
   a space, the caption text, and no full stop at the end.
4. A centred page number is added to the footer of every section, and a
   section break that closes the document is merged into the final section
   (so a landscape table at the end does not leave an empty page).
"""
import re, sys, zipfile, shutil, os, tempfile

src, dst = sys.argv[1], sys.argv[2]
appendix = "--appendix" in sys.argv

FOOTER = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  '<w:ftr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" '
  'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">'
  '<w:p><w:pPr><w:jc w:val="center"/></w:pPr><w:r><w:fldChar w:fldCharType="begin"/></w:r>'
  '<w:r><w:instrText xml:space="preserve"> PAGE </w:instrText></w:r><w:r><w:fldChar w:fldCharType="separate"/></w:r>'
  '<w:r><w:t>1</w:t></w:r><w:r><w:fldChar w:fldCharType="end"/></w:r></w:p></w:ftr>')
REL = '<Relationship Id="rIdPageNum" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer" Target="footer_pagenum.xml"/>'
CT = '<Override PartName="/word/footer_pagenum.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"/>'

tmp = tempfile.mktemp(suffix=".docx")
with zipfile.ZipFile(src) as zin, zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
    for item in zin.infolist():
        data = zin.read(item.filename)
        if item.filename == "word/document.xml":
            x = data.decode("utf-8")
            x, n1 = re.subn(r'<w:pPr><w:jc w:val="center" /></w:pPr>(?=<w:pPr>)', "", x)
            n2 = 0
            if appendix:
                x, n2 = re.subn(r'(<w:t[^>]*>)(?:Table|Fig\.|Figure)\s?\d+:\s+((?:Table|Fig\.) A\d+\.)', r"\1\2", x)
            def cap(m):
                p = m.group(0)
                p = re.sub(r'<w:r>(<w:rPr>(?:(?!</w:rPr>).)*</w:rPr>)?<w:t[^>]*>((?:Fig\.|Table)[ \u00a0]?A?\d+)[.:]?\s+',
                           lambda k: '<w:r><w:rPr><w:b/></w:rPr><w:t xml:space="preserve">' + k.group(2) + ' </w:t></w:r><w:r>' + (k.group(1) or '') + '<w:t xml:space="preserve">',
                           p, count=1, flags=re.S)
                p = re.sub(r'\.(</w:t>)(?!.*<w:t)', r'\1', p, flags=re.S)
                return p
            x, n3 = re.subn(r'<w:p>(?:(?!</w:p>).)*?w:val="(?:ImageCaption|TableCaption)"(?:(?!</w:p>).)*?</w:p>', cap, x, flags=re.S)
            print(f"{os.path.basename(dst)}: formatted {n3} captions")
            # a section break immediately before the body's closing sectPr becomes the final section
            x = re.sub(r'<w:p><w:pPr>(<w:sectPr>(?:(?!</w:sectPr>).)*</w:sectPr>)</w:pPr></w:p>\s*<w:sectPr\b(?:(?!</w:sectPr>).)*</w:sectPr>(\s*</w:body>)',
                       r'\1\2', x, flags=re.S)
            x, n4 = re.subn(r'<w:sectPr(\s[^>]*)?>(?!<w:footerReference)', lambda m: m.group(0) + '<w:footerReference w:type="default" r:id="rIdPageNum"/>', x)
            print(f"{os.path.basename(dst)}: page numbers in {n4} sections")
            data = x.encode("utf-8")
            print(f"{os.path.basename(dst)}: removed {n1} duplicate paragraph-property blocks, {n2} running caption numbers")
        if item.filename == "word/_rels/document.xml.rels":
            data = data.decode("utf-8").replace("</Relationships>", REL + "</Relationships>").encode("utf-8")
        if item.filename == "[Content_Types].xml":
            data = data.decode("utf-8").replace("</Types>", CT + "</Types>").encode("utf-8")
        zout.writestr(item, data)
    zout.writestr("word/footer_pagenum.xml", FOOTER)
shutil.move(tmp, dst)
