ERLC ?= erlc

all: compile

compile:
	$(ERLC) src/computor.erl

re: clean compile

clean:
	rm -rf *.beam erl_crash.dump

fclean: clean