.PHONY: clean
clean:
	rm -rf dist-newstyle
	rm -rf vhdl
	rm -rf netlist

format:
	stylish-haskell -c ./.stylish-haskell.yaml -r -i src tests

check_format:
	cp ./.stylish-haskell.yaml ./.stylish-haskell-check.yaml
	echo "exit_code: error_on_format" >> ./.stylish-haskell-check.yaml
	stylish-haskell -c ./.stylish-haskell-check.yaml -r src tests

clean_tests:
	rm ./.stylish-haskell-check.yaml

test:
	cabal test


HASKELL_SOURCES=$(shell find src -type f -iname '*.hs')

COPY = cp -a

vhdl=vhdl/GNodeBFAPI.topEntity/gnodeb_fapi_top.vhdl

${vhdl}: ${HASKELL_SOURCES}
	cabal run clash -- GNodeBFAPI --vhdl -g -fclash-clear

.PHONY: vhdl
vhdl: $(vhdl)

int:
	cabal run -- clashi

hoogle:
	hoogle server --local --port 8080
