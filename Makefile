nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

NAME=yaws_security
VERSION=0.1
RELEASE=1
OBJDIR=./obj
BASEDIR=$(OBJDIR)/$(NAME)-$(VERSION).$(RELEASE)
EBINDIR=$(BASEDIR)/ebin
DOCDIR=$(BASEDIR)/doc
SHEADERS += $(shell find include/*.hrl)
HEADERS = $(patsubst include/%.hrl,$(BASEDIR)/include/%.hrl,$(SHEADERS))
SRCS += $(shell find src/*.erl)
OBJS = $(patsubst src/%.erl,$(EBINDIR)/%.beam,$(SRCS))
WSMODULES = $(patsubst src/%.erl,%, $(SRCS))
MODULES = $(subst $(space),$(commaspace), $(WSMODULES))
INCLUDES += -I/usr/lib
INCLUDES += -I./include
EFLAGS += $(INCLUDES)
ifeq ($(TYPE),debug)
EFLAGS += -Ddebug +debug_info
endif
PKGS += -pa /usr/lib/yaws/ebin
PKGS += -pa $(EBINDIR)
RPMBIN=$(OBJDIR)/$(NAME)-$(VERSION).tar.gz
INSTPATH ?= /tmp

all: application doc tests

application: $(OBJS) $(EBINDIR)/$(NAME).app Makefile

$(BASEDIR)/include:
	@mkdir -p $(BASEDIR)/include

$(BASEDIR)/include/%.hrl: include/%.hrl $(BASEDIR)/include
	@cp $< $@

$(EBINDIR)/$(NAME).app: $(NAME).app Makefile $(HEADERS)
	@echo "Compiling (APPSPEC) $< to $@"
	cat $< | sed "s/__MODULES__/$(MODULES)/" | sed 's/\[, /\[/' \
	| sed "s/__VERSION__/$(VERSION).$(RELEASE)/g" > $@ 

$(OBJS): $(SRCS) Makefile

$(EBINDIR):
	@mkdir -p $(EBINDIR)

$(EBINDIR)/%.beam: src/%.erl Makefile $(EBINDIR)
	@touch $(NAME).app
	@echo "Compiling (Erlang) $< to $@"
	@erlc $(EFLAGS) -o $(EBINDIR) $<

tests: application
	@echo "Running unit tests.."
	@erl -noshell $(PKGS) \
	-eval "eunit:test(\"$(EBINDIR)\", [verbose])" \
	-yaws embedded true \
	 -s init stop

$(DOCDIR)/index.html: $(OBJS) Makefile
	@echo "Generating documentation.."
	erl -noshell $(PKGS) \
	-eval "edoc:application($(NAME), \".\", [{dir, \"$(DOCDIR)\"}])" \
	 -s init stop

doc: $(DOCDIR)/index.html

clean: 
	@rm -f *~
	@rm -f src/*~
	@rm -f include/*~
	@rm -rf obj
	@rm -f *.dump

obj/$(NAME).plt:	
	dialyzer --build_plt -r $(EBINDIR) --output_plt obj/$(NAME).plt

dialyzer: obj/$(NAME).plt
	dialyzer --plt obj/$(NAME).plt -r $(EBINDIR)

$(INSTPATH):
	@mkdir -p $(INSTPATH)

install: $(INSTPATH)
	@echo "Installing to $(INSTPATH)"
	@cp -r $(OBJDIR)/* $(INSTPATH)

$(OBJDIR)/$(NAME)-$(VERSION):
	@mkdir -p $(OBJDIR)/$(NAME)-$(VERSION)

$(OBJDIR)/$(NAME)-$(VERSION)/$(NAME).spec: $(OBJDIR)/$(NAME)-$(VERSION) $(NAME).spec Makefile
	@echo "Installing RPM specfile $@"
	@cat $(NAME).spec | sed -e 's/_RPM_VERSION/$(VERSION)/;s/_RPM_RELEASE/$(RELEASE)/' > $@

.PHONY: $(RPMBIN)

$(RPMBIN): $(OBJDIR)/$(NAME)-$(VERSION)/$(NAME).spec 
	tar -c --exclude=.git --exclude=*.spec --exclude=*~ --exclude=obj * | (cd $(OBJDIR)/$(NAME)-$(VERSION); tar xf -)
	(cd $(OBJDIR); tar cvz $(NAME)-$(VERSION)) > $(RPMBIN)

srcrpm: $(RPMBIN)
	rpmbuild -ts $(RPMBIN)
