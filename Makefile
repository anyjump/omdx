.PHONY: context

context:
	uvx files-to-prompt -e ml -e mli -c src/ | pbcopy
