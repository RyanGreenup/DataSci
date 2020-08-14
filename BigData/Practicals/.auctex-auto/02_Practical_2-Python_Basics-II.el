(TeX-add-style-hook
 "02_Practical_2-Python_Basics-II"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "minted"
    "/home/ryan/Dropbox/profiles/Templates/LaTeX/ScreenStyle")
   (LaTeX-add-labels
    "sec:org2a6c24c"
    "sec:org5adc63c"
    "sec:org6cf3875"
    "sec:orgb8571f8"
    "sec:orgf345f2e"
    "sec:org17d144e"
    "sec:orgb056ed7"
    "sec:org693e581"
    "sec:org1a10d9c"
    "sec:orgd4ca3fa"
    "sec:org044ba30"
    "sec:org1ba727f"
    "sec:org088dfe5"
    "sec:org4b7a218"
    "sec:orgc585340"
    "sec:orge5a1a9f"
    "sec:org60dc81a"
    "sec:org869de63"
    "sec:org73bad5f"
    "sec:org95b840b"
    "sec:org874d09b"
    "sec:org2014149"))
 :latex)

