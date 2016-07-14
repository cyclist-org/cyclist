all: index.html

index.html: index.txt #popl16.txt conference.html journal.html thesis.html other.html
	rst2html --no-compact-lists --stylesheet-path=styles.css -t --link-stylesheet index.txt index.html
#	rst2html --no-compact-lists --stylesheet-path=styles.css -t --link-stylesheet popl16.txt popl16.html	

#%.html: %.bib
#	bibtex2html -d -r -q -dl -unicode -nodoc -noheader $*.bib

clean:
	-rm -f *.html *~ 

