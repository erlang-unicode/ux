
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
LIBDIR:="lib/erlang/lib"
PKGNAME:="ux"
VERSION:="3.4.1"

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) skip_deps=true doc

eunit:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

check_plt:
	@$(REBAR) check-plt

dialyzer:
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

install:
        @for i in ebin/*.beam ebin/*.app; do install -m 644 -D $$i $(PREFIX)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
        cp -rf deps $(PREFIX)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/
        cp -rf priv $(PREFIX)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/

