PRESENTATION = four-temperaments

PANDOC = pandoc

default:
	$(PANDOC) -s -t dzslides -c new.css -o $(PRESENTATION).html $(PRESENTATION).md

revealjs:
	$(PANDOC) -s -t revealjs -V theme=simple -V transition=slide -V history=true --slide-level=2 --smart -o $(PRESENTATION).html $(PRESENTATION).md

dzslides:
	$(PANDOC) -s -t dzslides --slide-level=2 -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md petersen@fedorapeople.org:talks/fudcon-apac-2016/fp-intro-lab/
