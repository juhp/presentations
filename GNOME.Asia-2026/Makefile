PANDOC = pandoc

#
REVEAL_COMMON = -s -t revealjs -V transition=slide -V navigationMode=linear -V history=true -V slideNumber=true -V width=1280 -V height=720 --slide-level=2 --highlight-style=highlight-dark.theme
#--css local.css

REVEAL = $(REVEAL_COMMON) -V theme=black

# use the local reveal.js (works offline); 6.x keeps plugins under dist/plugin/
reveal: reveal.js
	$(PANDOC) $(REVEAL) -V revealjs-url=reveal.js -o talk.html index.md
	sed -i -E 's|reveal.js/plugin/([a-z]+)/[a-z]+\.js|reveal.js/dist/plugin/\1.js|' talk.html

slidy:
	$(PANDOC) -s -t slidy -o talk.html index.md

self:
	$(PANDOC) $(REVEAL) --self-contained -o index.html index.md

upload:
	$(PANDOC) $(REVEAL) --self-contained -o index.html index.md
	scp index.html index.md fedorapeople.org:talks/gnome-asia-2026-isolation/

REVEAL_VER = 6.0.2

reveal.js: $(REVEAL_VER).tar.gz
	tar xf $(REVEAL_VER).tar.gz
	ln -s reveal.js-$(REVEAL_VER) reveal.js

$(REVEAL_VER).tar.gz:
	wget https://github.com/hakimel/reveal.js/archive/$(REVEAL_VER).tar.gz

pdf:
	$(PANDOC) -s -t slidy  index.md | weasyprint -s pdf.css - index.pdf
