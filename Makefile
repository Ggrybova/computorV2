ERLC ?= erlc

all: compile

compile:
	$(ERLC) computor.erl

re: clean compile

clean:
	rm -rf *.beam erl_crash.dump

fclean: clean