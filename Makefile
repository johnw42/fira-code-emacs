# Modified font files.  Copy these into your ~/.fonts directory and
# choose one as your default Emacs font.
FIRA_EMACS_FONTS = \
	modified/FiraEmacs-Light.ttf \
	modified/FiraEmacs-Regular.ttf \
	modified/FiraEmacs-Bold.ttf \
	modified/FiraEmacs-Medium.ttf \
	modified/FiraEmacs-Retina.ttf

ZIP_FILE = Fira_Code_v5.zip
FIRA_URL = https://github.com/tonsky/FiraCode/releases/download/5/$(ZIP_FILE)
CASCADIA_ZIP = CascadiaCode_2005.15.zip
CASCADIA_URL = https://github.com/microsoft/cascadia-code/releases/download/v2005.15/$(CASCADIA_ZIP)

all: fira-all cascadia-all

fira-all: $(FIRA_EMACS_FONTS) fira-code-data.el

cascadia-all: modified/CascadiaEmacs.ttf cascadia-code-data.el

clean:
	rm -f $(ZIP_FILE) fontTools-stamp $(CASCADIA_ZIP)
	rm -rf original modified

distclean:
	rm -rf *-code-data.el

# Ugly hack.  Building any font also makes fira-code-data.el as a side-effect.
fira-code-data.el: modified/FiraEmacs-Medium.ttf

modified/FiraEmacs-%.ttf: original/ttf/FiraCode-%.ttf build_fira_emacs.py fontTools-stamp
	mkdir -p modified
	python3 build_fira_emacs.py $< $@ fira-code-data.el

modified/CascadiaEmacs.ttf cascadia-code-data.el: original/CascadiaCode.ttf build_cascadia_emacs.py fontTools-stamp
	mkdir -p modified
	python3 build_cascadia_emacs.py $< $@ cascadia-code-data.el

fontTools-stamp:
	pip3 install --user fontTools
	touch $@

$(ZIP_FILE):
	curl -L -o $@ $(FIRA_URL)

original/ttf/FiraCode-%.ttf: $(ZIP_FILE)
	mkdir -p original
	unzip -u $(ZIP_FILE) ttf/FiraCode-$*.ttf -d original
	touch $@

original/CascadiaCode.ttf: $(CASCADIA_ZIP)
	mkdir -p original
	cd original \
		&& unzip -u ../$(CASCADIA_ZIP) ttf/CascadiaCode.ttf \
		&& mv ttf/CascadiaCode.ttf . \
		&& rmdir --ignore-fail-on-non-empty ttf

$(CASCADIA_ZIP):
	curl -L -o $@ $(CASCADIA_URL)
