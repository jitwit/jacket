jconsole:= /Applications/j901/bin/jconsole
racket:= /Applications/Racket\ v7.7/bin/racket
nuvoc-url:= https://code.jsoftware.com/wiki/NuVoc

default : NuVoc.el

NuVoc.html :
	$(jconsole) -js "require 'web/gethttp'" "exit '-o $@' gethttp '$(nuvoc-url)'"

NuVoc.el : nuvoc.rkt NuVoc.html
	$(racket) $<

clean :
	rm -rf *~
