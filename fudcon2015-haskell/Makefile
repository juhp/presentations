PRESENTATION = haskell-good-for-us

all:
	pandoc -s -t revealjs -V theme=simple -V transition=none -o $(PRESENTATION).html $(PRESENTATION).md
#	pandoc -s -t dzslides -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md petersen@fedorapeople.org:public_html/talks/fudcon-pune-2015/
