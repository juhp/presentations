PANDOC = pandoc

REVEAL = -s -t revealjs -V theme=night -V transition=slide -V history=true --slide-level=2 --css local.css

reveal: reveal.js
	$(PANDOC) $(REVEAL) -o talk.html index.md

slidy:
	$(PANDOC) -s -t slidy -o talk.html index.md

self:
	$(PANDOC) $(REVEAL) --self-contained -o index.html index.md

upload:
	$(PANDOC) $(REVEAL) --self-contained -o index.html index.md
	scp index.html index.md fedorapeople.org:talks/haskellsg-2020-distros/

REVEAL_VER = 3.9.2

reveal.js: ../$(REVEAL_VER).tar.gz
	tar xf ../$(REVEAL_VER).tar.gz
	ln -s reveal.js-$(REVEAL_VER) reveal.js

../$(REVEAL_VER).tar.gz:
	wget --directory-prefix=.. https://github.com/hakimel/reveal.js/archive/$(REVEAL_VER).tar.gz

pdf:
	$(PANDOC) -s -t slidy  index.md | weasyprint -s pdf.css - index.pdf
