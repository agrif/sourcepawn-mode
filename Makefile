.PHONY : all clean scan generate dist

VERSION=0.1

all : scan generate

clean :
	rm sourcepawn-mode.el
	rm keywords/generated/*.txt

generate :
	emacs --script tools/generate.el

scan :
	emacs --script tools/scan.el

# make tarballs, zips, ...
dist : clean all
	mkdir /tmp/sourcepawn-mode-${VERSION}
	cp -r ./* /tmp/sourcepawn-mode-${VERSION}
	rm -f /tmp/sourcepawn-mode-${VERSION}/sp-include/*
	mv /tmp/sourcepawn-mode-${VERSION} ./
	zip -r sourcepawn-mode-${VERSION}.zip sourcepawn-mode-${VERSION}
	tar -czvf sourcepawn-mode-${VERSION}.tar.gz sourcepawn-mode-${VERSION}
	tar -cjvf sourcepawn-mode-${VERSION}.tar.bz2 sourcepawn-mode-${VERSION}
	rm -rf sourcepawn-mode-${VERSION}
