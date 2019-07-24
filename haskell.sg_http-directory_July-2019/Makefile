PANDOC = pandoc

OPTS = -t slidy

html: index.md
	$(PANDOC) -s $(OPTS) -o talk.html index.md

pdf:
	$(PANDOC) -s $(OPTS) index.md | weasyprint -s local.css - index.pdf

upload:
	$(PANDOC) -s $(OPTS) --self-contained -o index.html index.md
	scp index-web.html index.md fedorapeople.org:talks/haskell-sg-201907-http-directory/
