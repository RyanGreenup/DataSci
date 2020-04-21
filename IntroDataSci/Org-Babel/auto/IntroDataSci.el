(TeX-add-style-hook
 "IntroDataSci"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "sec:orgad9bff5"
    "sec:orge805c55"
    "sec:org058c0ed"
    "sec:org7a71359"
    "sec:orgd878f52"
    "sec:org0ca3ddf"
    "sec:org6bb64bc"
    "sec:org7d602a0"
    "sec:orgdd060df"
    "sec:orgf86bbd2"
    "sec:org5126b6d"
    "sec:orgb335fb8"
    "sec:org1c2b8d6"
    "sec:orga5b4169"
    "sec:orgc6ada8f"
    "sec:orgc595d81"
    "sec:org9374a53"
    "sec:orgccf5811"
    "sec:orgce72af0"
    "sec:org9c8dfc0"
    "sec:org971a574"
    "sec:orge202012"
    "sec:orgd6856d0"
    "sec:org14d95a7"
    "sec:org8cb53e8"
    "sec:org83ed81c"
    "sec:orgb6099fd"
    "sec:org40327bb"
    "sec:orgdb41fa8"
    "sec:org31ab181"
    "sec:org9a78e63"
    "sec:orgc666646"
    "sec:org6309502"
    "sec:orgc6c919e"
    "sec:orgbb30466"
    "sec:org80f4599"
    "sec:org242e409"
    "sec:orgf99150e"
    "sec:orgd8baea4"
    "sec:org4e7e442"
    "sec:org2d0fda3"
    "sec:org1022a71"
    "sec:org6f199ff"
    "sec:orgd1fb469"
    "sec:orge5ea163"
    "sec:org92d1a40"
    "sec:org4bec9a1"
    "sec:org990aad3"
    "sec:org831a573"
    "sec:org16d78cc"
    "sec:org196f8f2"
    "sec:orgcaf68ea"
    "sec:orgce7475e"
    "sec:orge6d095e"
    "sec:orgcb26af2"
    "sec:org855cc29"
    "sec:orgc927eab"
    "sec:orga06e321"
    "sec:org1afd5bd"
    "sec:org1581a7b"
    "sec:org7fd3f26"
    "sec:orga8baa5a"
    "sec:orgdcff267"
    "sec:orga31d01d"
    "sec:org46b2268"
    "sec:orgda1638b"
    "sec:org1bd734f"))
 :latex)

