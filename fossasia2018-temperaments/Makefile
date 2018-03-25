PRESENTATION = four-temperaments

PANDOC = pandoc

default:
	$(PANDOC) -s -f markdown+header_attributes -t revealjs -c temperament.css -V theme=simple-juhp -V transition=fade -V history=true --slide-level=1 -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md *.css petersen@fedorapeople.org:talks/fossasia-2018/
