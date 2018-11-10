ERLC ?= erlc

all: compile

compile:
	$(ERLC) src/computor.erl src/quadratic_equations.erl src/my_lib.erl


re: clean compile

clean:
	rm -rf *.beam erl_crash.dump

fclean: clean