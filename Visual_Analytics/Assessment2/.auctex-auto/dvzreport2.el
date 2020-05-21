(TeX-add-style-hook
 "dvzreport2"
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
    "sec:org65e16aa"
    "sec:org32a0c5d"
    "sec:orgb2f25d9"
    "sec:org8c24a48"
    "sec:org78c2c85"
    "sec:org41a2a34"
    "sec:org83a8edb"
    "sec:org7accacb"
    "sec:orge18402f"
    "sec:org7a533fc"
    "sec:org9ad7bb1"
    "org250a22c"
    "org70648f3"
    "org75f40f0"
    "org7ccb826"
    "orgbde88c8"
    "fig:org7e64dfd"
    "sec:org70b837a"
    "orga5bd32f"
    "org181dd8d"
    "org8902b95"
    "fig:org223d165"
    "sec:orgd18e079"
    "exp1"
    "exp2"
    "orgda7745e"
    "sec:org190a922"
    "sec:orgd3a1d68"
    "sec:org09b797c"
    "sec:org32481c2"
    "sec:org5cd481d"
    "sec:orgdb93e1d"
    "sec:org574a540"
    "sec:org7a659d1"
    "sec:org1cd00da"
    "sec:org086c831"
    "sec:org246372d"
    "sec:orgef591b1"
    "sec:orgb998fb8"
    "sec:org03453be"
    "sec:org0f605b9"
    "sec:orgda0a6ca"
    "sec:org62ec7d4"
    "sec:orgd1d052a"
    "sec:org6b274e8"
    "sec:org68719b3"
    "sec:org99999c8"
    "sec:org51b751c"
    "sec:orge59d4df"
    "sec:orgcf32daa"
    "sec:org9c68d96"
    "sec:org805633b"
    "sec:org2c67c23"
    "sec:orgfc48c02"
    "sec:org077d0b0"
    "fig:org615a27a"
    "sec:org2fdbae4"
    "org89765da"
    "org655b7fa")
   (LaTeX-add-bibliographies
    "../../../../Studies/Papers/references"))
 :latex)

