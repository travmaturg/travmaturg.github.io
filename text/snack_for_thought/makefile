.POSIX:

md2html = pandoc -f markdown_strict -t html4 -s

snack_for_thought.html : snack_for_thought.md
	$(md2html) -o $@ $^
