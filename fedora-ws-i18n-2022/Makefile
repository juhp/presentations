PANDOC = pandoc

slidy:
	$(PANDOC) -s -t slidy -o ws-i18n-202205.html ws-i18n-202205.md

html:
	$(PANDOC) -s -o ws-i18n-202205-single.html ws-i18n-202205.md

upload: slidy html
	scp ws-i18n-202205.md ws-i18n-202205*.html fedorapeople.org:public_html/
