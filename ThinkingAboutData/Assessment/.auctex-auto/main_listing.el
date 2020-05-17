(TeX-add-style-hook
 "main_listing"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("geometry" "margin=1in") ("biblatex" "citestyle=numeric" "bibstyle=numeric" "hyperref=true" "backref=true" "maxcitenames=3" "url=true" "backend=biber" "natbib=true")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "lmodern"
    "amssymb"
    "amsmath"
    "ifxetex"
    "ifluatex"
    "fontenc"
    "inputenc"
    "textcomp"
    "unicode-math"
    "upquote"
    "microtype"
    "parskip"
    "xcolor"
    "xurl"
    "bookmark"
    "hyperref"
    "listings"
    "longtable"
    "booktabs"
    "etoolbox"
    "footnotehyper"
    "footnote"
    "graphicx"
    "pgfplots"
    "comment"
    "import"
    "tikz"
    "pstricks"
    "sectsty"
    "enumitem"
    "lipsum"
    "geometry"
    "amsthm"
    "biblatex"
    "bibentry"
    "titlesec")
   (TeX-add-symbols
    '("subtitle" 1)
    '("passthrough" 1)
    "tightlist"
    "maxwidth"
    "maxheight")
   (LaTeX-add-labels
    "declaration"
    "preliminary"
    "load-packages"
    "inspect-and-clean-data"
    "question-1"
    "plot"
    "observations-from-plot"
    "analysis-and-results"
    "hypothesis"
    "test-statistic"
    "F"
    "ssb"
    "ssw"
    "rejection-region"
    "statistic"
    "conclusion"
    "question-2"
    "plot-1"
    "observations-from-plot-1"
    "analysis-and-results-1"
    "students-t-distribution"
    "ttest"
    "simulation"
    "conclusion-1"
    "question-3"
    "plot-2"
    "observations-from-plot-2"
    "analysis-and-results-2"
    "conclusion-2"
    "question-4"
    "plot-3"
    "observations-from-plot-3"
    "analysis-and-results-3"
    "question-5"
    "plot-4"
    "expmod"
    "observations-from-plot-4"
    "analysis-and-results-4"
    "hypothesis-1"
    "test-statistic-1"
    "conclusion-3")
   (LaTeX-add-environments
    '("sol" LaTeX-env-args ["argument"] 0))
   (LaTeX-add-bibliographies
    "/home/ryan/Dropbox/Studies/Papers/references")
   (LaTeX-add-xcolor-definecolors
    "colsse"
    "colsss"
    "colsubsub"
    "colspg"
    "colsbpg"
    "coltit"
    "colname"))
 :latex)

