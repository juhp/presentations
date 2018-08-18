PRESENTATION = devconf.us-2018-fedora-haskell

PANDOC = pandoc

OPTS = -t revealjs -V theme=night -V transition=slide -V history=true --slide-level=2 --css local.css

html: reveal.js
	$(PANDOC) -s $(OPTS) -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	$(PANDOC) -s $(OPTS) --self-contained -o $(PRESENTATION)-web.html $(PRESENTATION).md
	scp $(PRESENTATION)-web.html $(PRESENTATION).md fedorapeople.org:talks/devconf.us-2018-fedora-haskell/

REVEAL = 3.7.0

reveal.js:
	wget https://github.com/hakimel/reveal.js/archive/$(REVEAL).tar.gz
	tar zf reveal.js-$(REVEAL).tar.gz
	ln -s reveal.js-$(REVEAL) reveal.js
