PRESENTATION = fudconpp-fp-intro

#PANDOC = $(HOME)/copr/pandoc/pandoc-1.16.0.2/.cabal-sandbox/bin/pandoc
PANDOC = pandoc

default: revealjs


revealjs:
	$(PANDOC) -s -t revealjs -V theme=simple -V transition=slide -V history=true --slide-level=2 --smart -o $(PRESENTATION).html $(PRESENTATION).md

dzslides:
	$(PANDOC) -s -t dzslides --slide-level=2 -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md petersen@fedorapeople.org:talks/fudcon-apac-2016/fp-intro-lab/
