PRESENTATION = haskell-good-for-us

default: revealjs

revealjs:
	pandoc -s -t revealjs -V theme=simple -V transition=slide -V history=true --slide-level=2 --smart -o $(PRESENTATION).html $(PRESENTATION).md

dzslides:
	pandoc -s -t dzslides --slide-level=2 --smart -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md petersen@fedorapeople.org:public_html/talks/fudcon-pune-2015/
