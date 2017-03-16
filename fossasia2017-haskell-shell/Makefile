PRESENTATION = haskell-shell-scripting

PANDOC = pandoc

PRODUCTION = --self-contained 

slidy:
	$(PANDOC) -s -t slidy --smart -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md juhp@code.haskell.org:talks/fossasia-2017-haskell-shell/
