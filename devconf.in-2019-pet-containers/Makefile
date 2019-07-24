PANDOC = pandoc

REVEAL = -s -t revealjs -V theme=night -V transition=slide -V history=true --slide-level=2 --css local.css

reveal: reveal.js
	$(PANDOC) $(REVEAL) -o talk.html index.md

slidy:
	$(PANDOC) -s -t slidy -o talk.html index.md

upload:
	$(PANDOC) $(REVEAL) --self-contained -o index.html index.md
	scp index.html index.md fedorapeople.org:talks/devconf.in-2019-pet-containers/

REVEAL_VER = 3.8.0

reveal.js: $(REVEAL_VER).tar.gz
	tar xf $(REVEAL_VER).tar.gz
	ln -s reveal.js-$(REVEAL_VER) reveal.js

$(REVEAL_VER).tar.gz:
	wget https://github.com/hakimel/reveal.js/archive/$(REVEAL).tar.gz

pdf:
	$(PANDOC) -s -t slidy  index.md | weasyprint -s pdf.css - index.pdf
