# fira-code-emacs

Some code to make Fira Code 2.0 work in Emacs.

To install, first run `make`.  Copy some or all of the files in
`modified` to you `~/.fonts` directory.  Put `fira-code.el` and
`fira-code-data.el` into your `load-path`, and ensure `fira-code-mode`
is enabled for any buffers where you want ligatures.  Set your default
font to one of the Fira Emacs variants.  If you want to see all the
ligatures variant characters, open `fira-code-data.el` in Emacs.

To control which ligatures and alternative characters are used, set
`fira-code-enable-substitution-predicate` and
`fira-code-compose-predicate`.  Refer to the default values for an
example of how to write your own predicates.
