.PHONY : all clean scan generate

all : scan generate

clean :
	rm sourcepawn-mode.el
	rm keywords/generated/*.txt

generate :
	emacs --script tools/generate.el

scan :
	emacs --script tools/scan.el
