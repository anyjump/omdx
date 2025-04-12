.PHONY: ctx-src ctx-ppx ctx

ctx-src:
	uvx files-to-prompt -e ml -e mli -c src/ | pbcopy

ctx-ppx:
	uvx files-to-prompt -e ml -e mli -c ppx/ | pbcopy

ctx:
	uvx files-to-prompt -e ml -e mli -c src/ tests/ ppx/ | pbcopy

test:
	dune runtest -w
