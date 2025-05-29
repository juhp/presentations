PANDOC = pandoc

NAME=flock-2025-tools

slidy:
	$(PANDOC) -s -t slidy -o $(NAME).html $(NAME).md

html:
	$(PANDOC) -s -o $(NAME)-single.html $(NAME).md

upload: slidy html
	scp $(NAME)* fedorapeople.org:public_html/
