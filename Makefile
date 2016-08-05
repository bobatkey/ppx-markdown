.DEFAULT_GOAL := all
.PHONY: all test clean install uninstall

all: ppx_markdown.native

ppx_markdown.native: src/*.ml
	ocamlbuild \
      -use-ocamlfind \
      -package compiler-libs \
      -package ppx_tools.metaquot \
      -package omd \
      src/ppx_markdown.native

# test.native: ppx_markdown.native test/*.ml
# 	ocamlbuild \
#       -use-ocamlfind \
#       -package oUnit \
#       -cflags -ppx,../ppx_markdown.native \
#       test/test.native

install: ppx_markdown.native META
	@ocamlfind install ppx_markdown META ppx_markdown.native

uninstall:
	@ocamlfind remove ppx_markdown

# test: test.native
# 	@./test.native

clean:
	rm -rf _build
	rm -f ppx_markdown.native
	rm -f test.native
	rm -f META
	rm -f oUnit-anon.cache

PKG_VERSION := 0.2

META: META.in
	@sed 's/$$(pkg_version)/$(PKG_VERSION)/g' < $< > $@
