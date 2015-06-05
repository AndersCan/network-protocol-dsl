while [ true ]
do
  pdflatex head.tex
  bibtex head.aux
  bibtex head.aux
  pdflatex head.tex
  pdflatex head.tex
  sleep 30
done
	
