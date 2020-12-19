FILE = hake_jtc_data_us.utf8
ENGINE = lualatex
YEAR = `date +'%Y'`

pdf:
	pandoc -t beamer -o $(FILE).pdf $(FILE).md --lua-filter=tagged-filter.lua --pdf-engine=$(ENGINE) --template=tagged-template.latex

tex:
	pandoc -t beamer -o $(FILE).tex $(FILE).md --lua-filter=tagged-filter.lua --template=tagged-template.latex

pdfnb: tex
	sed -i '/^$$/d' $(FILE).tex
	rm -f $(FILE).xmpdata
	$(ENGINE) $(FILE).tex

final: pdfnb
	mv $(FILE).pdf "JTC Information on US Fishery Dec_"$(YEAR).pdf
