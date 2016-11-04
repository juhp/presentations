PRESENTATION = index

html:
	pandoc -s -t html -o $(PRESENTATION).html $(PRESENTATION).md

upload:
	scp $(PRESENTATION).html $(PRESENTATION).md petersen@fedorapeople.org:talks/fudcon-apac-2016/
