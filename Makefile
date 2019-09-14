# Modified font files.  Copy these into your ~/.fonts directory and
# choose one as your default Emacs font.
EMACS_FONTS = \
	modified/FiraEmacs-Light.otf \
	modified/FiraEmacs-Regular.otf \
	modified/FiraEmacs-Bold.otf \
	modified/FiraEmacs-Medium.otf \
	modified/FiraEmacs-Retina.otf

ZIP_FILE = FiraCode_2.zip
URL = https://github.com/tonsky/FiraCode/releases/download/2/$(ZIP_FILE)

all: $(EMACS_FONTS)

clean:
	rm -f $(ZIP_FILE) fontTools-stamp
	rm -rf original modified

distclean:
	rm -rf fira-code-data.el

modified/FiraEmacs-%.otf: original/otf/FiraCode-%.otf build_fira_emacs.py fontTools-stamp
	mkdir -p modified
	python3 build_fira_emacs.py $< $@ fira-code-data.el

fontTools-stamp:
	pip3 install --user fontTools
	touch $@

$(ZIP_FILE):
	curl -L -o $@ $(URL)

original/otf/FiraCode-%.otf: $(ZIP_FILE)
	unzip -f $(ZIP_FILE) otf/FiraCode-$*.otf -d original
	touch $@

