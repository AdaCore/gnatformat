.PHONY: all
all: plugin parser

.PHONY: parser
parser:
	gprbuild -P gnat/ada_ast.gpr

.PHONY: plugin
plugin:
	npm run build

.PHONY: setup
setup:
	rm -rf node_modules
	npm install

.PHONY: clean
clean:
	rm -rf dist/*
	rm -rf obj/*