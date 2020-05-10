jconsole:= /Applications/j901/bin/jconsole
racket:= /Applications/Racket\ v7.7/bin/racket
nuvoc-url:= https://code.jsoftware.com/wiki/NuVoc

default : data/j.sexp

data/NuVoc :
	$(jconsole) -js "require 'web/gethttp'" "exit '-o $@' gethttp '$(nuvoc-url)'"

data/j.sexp : nuvoc.rkt data/NuVoc
	$(racket) $<

clean :
	rm -rf *~
