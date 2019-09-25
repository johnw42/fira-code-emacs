# Modified font files.  Copy these into your ~/.fonts directory and
# choose one as your default Emacs font.
FIRA_EMACS_FONTS = \
	modified/FiraEmacs-Light.otf \
	modified/FiraEmacs-Regular.otf \
	modified/FiraEmacs-Bold.otf \
	modified/FiraEmacs-Medium.otf \
	modified/FiraEmacs-Retina.otf	

ZIP_FILE = FiraCode_2.zip
FIRA_URL = https://github.com/tonsky/FiraCode/releases/download/2/$(ZIP_FILE)

all: fira-all cascadia-all

fira-all: $(FIRA_EMACS_FONTS) fira-code-data.el

cascadia-all: modified/CascadiaEmacs.ttf cascadia-code-data.el

clean:
	rm -f $(ZIP_FILE) fontTools-stamp
	rm -rf original modified

distclean:
	rm -rf *-code-data.el

# Ugly hack.  Building any font also makes fira-code-data.el as a side-effect.
fira-code-data.el: modified/FiraEmacs-Medium.otf

modified/FiraEmacs-%.otf: original/otf/FiraCode-%.otf build_fira_emacs.py fontTools-stamp
	mkdir -p modified
	python3 build_fira_emacs.py $< $@ fira-code-data.el

modified/CascadiaEmacs.ttf cascadia-code-data.el: original/Cascadia.ttf build_cascadia_emacs.py fontTools-stamp
	mkdir -p modified
	python3 build_cascadia_emacs.py $< $@ cascadia-code-data.el

fontTools-stamp:
	pip3 install --user fontTools
	touch $@

$(ZIP_FILE):
	curl -L -o $@ $(FIRA_URL)

original/otf/FiraCode-%.otf: $(ZIP_FILE)
	mkdir -p original
	unzip -u $(ZIP_FILE) otf/FiraCode-$*.otf -d original
	touch $@

original/Cascadia.ttf:
	mkdir -p original
	curl -L -o $@ https://github.com/microsoft/cascadia-code/releases/download/v1909.16/Cascadia.ttf
