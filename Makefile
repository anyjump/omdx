.PHONY: ctx-lib ctx-ppx ctx

ctx-lib:
	uvx files-to-prompt -e ml -e mli -c src/ | pbcopy

ctx-ppx:
	uvx files-to-prompt -e ml -e mli -c ppx/ | pbcopy

ctx:
	uvx files-to-prompt -e ml -e mli -c src/ ppx/ test/ | pbcopy

test:
	dune runtest -w
