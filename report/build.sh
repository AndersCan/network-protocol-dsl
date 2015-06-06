while [ true ]
do
  pdflatex -interaction nonstopmode head.tex
  bibtex head.aux
  bibtex head.aux
  pdflatex -interaction nonstopmode head.tex
  pdflatex -interaction nonstopmode head.tex
  sleep 30
done
	
