PRESENTATION = four-temperaments

PANDOC = pandoc

default:
	$(PANDOC) -s -t revealjs -c temperament.css -V theme=simple-juhp -V transition=fade -V history=true --slide-level=2 -o $(PRESENTATION).html $(PRESENTATION).md

dzslides:
	$(PANDOC) -s -t dzslides --slide-level=2 -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md petersen@fedorapeople.org:talks/fossasia-2018
