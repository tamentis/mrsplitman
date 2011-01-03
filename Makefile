all: irc.beam splitter.beam
	true

irc.beam: irc.erl
	erlc irc.erl

splitter.beam: splitter.erl
	erlc splitter.erl

start:
	erl -noshell -run irc

clean:
	rm -f *.beam
