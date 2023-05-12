.PHONY: dev
dev: # compiles and opens
	elm make src/Main.elm
	open -a Safari index.html

.PHONY: build
build: #just compiles
	elm make src/Main.elm
