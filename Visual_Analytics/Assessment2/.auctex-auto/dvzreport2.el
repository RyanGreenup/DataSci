(TeX-add-style-hook
 "dvzreport2"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
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
    "/home/ryan/Dropbox/profiles/Templates/LaTeX/ScreenStyleDV")
   (LaTeX-add-labels
    "sec:org956f2bb"
    "sec:org335e89d"
    "sec:orgb8553bf"
    "sec:org87a2ce8"
    "sec:org39241de"
    "org8d57e1b"
    "sec:orgba27346"
    "org5b07650"
    "sec:org3a2b75d"
    "org420cba7"
    "org44f7f71"
    "orgef197ce"
    "org4e31ba3"
    "orgea1e218"
    "fig:orgbde4873"
    "orgdf4f16f"
    "fig:orga1d9f48"
    "sec:orgfbfc7a1"
    "orga17bfdd"
    "orgc0543c1"
    "orgf7a336d"
    "fig:org8276add"
    "sec:org75a6c83"
    "sec:org13220d1"
    "sec:orge45e1ab"
    "sec:orgab47f26"
    "sec:org659d7a5"
    "sec:org4a86e6b"
    "org8f1cfd9"
    "sec:org4957c46"
    "sec:org7595a19"
    "sec:orge5a6140"
    "exp1"
    "exp2"
    "sec:orgfa6e39c"
    "orgb846a90"
    "sec:org422c64f"
    "sec:org7ab654c"
    "org4691f98"
    "tab:orge7eb163"
    "org5bb4fe3"
    "fig:org4bbeb34"
    "sec:org747b052"
    "org2af87a5"
    "org51fccd9"
    "orgbed3112"
    "orge5f6201"
    "orgb13ebcd"
    "org4c22a85"
    "org58d5812"
    "fig:org542b788"
    "sec:orgb1bf3c2"
    "sec:org29ecbad"
    "sec:orgdc0628a"
    "sec:orge8291b8"
    "sec:org40c1895"
    "sec:orgb87d931"
    "sec:org3171162"
    "sec:org37574a5"
    "orgeb27ca5"
    "fig:org244afa4"
    "sec:orgb5a39e8"
    "fig:org2e5afd2"
    "fig:orge888b39"
    "fig:orgb7dd52e"
    "orgc761d89"
    "sec:org63fafc1")
   (LaTeX-add-environments
    '("sol" LaTeX-env-args ["argument"] 0)))
 :latex)

