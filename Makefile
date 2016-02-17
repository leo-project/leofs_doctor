.PHONY: deps compile xref eunit check_plt build_plt dialyzer typer doc callgraph graphviz clean distclean qc

REBAR := ./rebar
RELX := ./relx
APPS = erts kernel stdlib sasl crypto compiler inets mnesia
LIBS = deps/cecho/ebin
PLT_FILE = .leofs_doctor_dialyzer_plt
DOT_FILE = leofs_doctor.dot
CALL_GRAPH_FILE = leofs_doctor.png

all: deps
	@$(REBAR) compile
	@$(REBAR) xref skip_deps=true
	@$(REBAR) eunit skip_deps=true
	@$(RELX)
release:
	@$(RELX)
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile skip_deps=true
xref:
	@$(REBAR) xref skip_deps=true
eunit:
	@$(REBAR) eunit skip_deps=true
check_plt:
	@$(REBAR) compile
	dialyzer --check_plt --plt $(PLT_FILE) --apps $(APPS)
build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(PLT_FILE) --apps $(APPS) $(LIBS)
dialyzer:
	@$(REBAR) compile
	dialyzer --plt $(PLT_FILE) -r ebin/ --dump_callgraph $(DOT_FILE) | fgrep -v -f ./dialyzer.ignore-warnings
typer:
	typer --plt $(PLT_FILE) -I include/ -r src/
doc: compile
	@$(REBAR) doc
callgraph: graphviz
	dot -Tpng -o$(CALL_GRAPH_FILE) $(DOT_FILE)
graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))
clean:
	@$(REBAR) clean skip_deps=true
distclean:
	@$(REBAR) delete-deps
	@$(REBAR) clean
qc:
	@$(REBAR) qc skip_deps=true
