PANDOC = pandoc

NAME=f44-release-party-nix

# REVEAL
MODE = -s -t revealjs -V theme=night -V transition=slide -V history=true --slide-level=2 --css local.css
# MODE = -t slidy

$(NAME).html: $(NAME).md
	$(PANDOC) $(MODE) -i -o $(NAME).html $(NAME).md

$(NAME)-single.html: $(NAME).md
	$(PANDOC) -s -o $(NAME)-single.html $(NAME).md

upload: $(NAME)-single.html
	$(PANDOC) $(MODE) --embed-resources -o $(NAME).html $(NAME).md
	scp $(NAME)*.md $(NAME)*.html fedorapeople.org:public_html/
