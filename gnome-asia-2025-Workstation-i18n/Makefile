SLIDES = gnome-asia-2025-fedora-ws-i18n

upload: $(SLIDES).pdf
	scp $(SLIDES).* fedorapeople.org:talks/

$(SLIDES).pdf: $(SLIDES).odp
	flatpak run org.libreoffice.LibreOffice --headless --convert-to pdf $(SLIDES).odp
