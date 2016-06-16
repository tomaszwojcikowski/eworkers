PROJECT = eworkers

RELX_OUTPUT_DIR = _rel

EXOMETER_PKGS=EXOMETER_PACKAGES="(minimal)"

REBAR=$(EXOMETER_PKGS) ./rebar

all: get-deps compile rel

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

app:
	$(REBAR) compile skip_deps=true

shell:
	erl -pa ebin deps/*/ebin -s tengi_amqp

rel: clean_relx
	./relx -c relx.config -o $(RELX_OUTPUT_DIR)

clean: clean_relx clean_ct
	$(REBAR) clean
	$(REBAR) delete-deps

clean_relx:
	rm -rf $(RELX_OUTPUT_DIR)

clean_ct:
	rm -rf test/*.beam

tests: eunit

eunit:
	$(REBAR) -v eunit skip_deps=true

ct:
	$(REBAR) -v ct skip_deps=true

meck: $(DEPS_DIR)/meck

exometer: $(DEPS_DIR)/exometer
	cd $(DEPS_DIR)/exometer; make compile
