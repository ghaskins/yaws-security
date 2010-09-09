nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

NAME=yaws_security
OBJDIR=./obj
EBINDIR=$(OBJDIR)/$(NAME)/ebin
DOCDIR=$(OBJDIR)/$(NAME)/doc
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

all: application release

application: $(OBJS) $(EBINDIR)/$(NAME).app Makefile

release: $(OBJDIR)/$(NAME).boot tests doc Makefile

$(EBINDIR)/$(NAME).app: $(NAME).app Makefile
	@echo "Compiling (APPSPEC) $< to $@"
	@cat $< | sed "s/__MODULES__/$(MODULES)/" | sed 's/\[, /\[/' > $@ 

$(OBJS): $(SRCS) Makefile

$(EBINDIR)/%.beam: src/%.erl Makefile
	@touch $(NAME).app
	@echo "Compiling (Erlang) $< to $@"
	@mkdir -p $(EBINDIR)
	@erlc $(EFLAGS) -o $(EBINDIR) $<

$(OBJDIR)/$(NAME).boot: $(NAME).rel application
	@echo "Compiling (Release) $< to $@"
	@mkdir -p $(OBJDIR)
	@erlc $(PKGS) -o $(OBJDIR) $<

tests:
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
