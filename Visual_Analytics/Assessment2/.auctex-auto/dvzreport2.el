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
    "sec:org4df1a26"
    "sec:org3fd505b"
    "sec:org225660a"
    "sec:org00c0e47"
    "sec:org8153164"
    "sec:orgb778a6e"
    "sec:org1f4bcf8"
    "sec:orga81a162"
    "sec:org86bbed3"
    "sec:orgcbda394"
    "sec:org2fa4c82"
    "org347d4df"
    "orgefdab1b"
    "org16dd651"
    "org19102a6"
    "orga811027"
    "fig:orgb9d1c6a"
    "org85a4e90"
    "fig:org9c4f0e3"
    "sec:orgd9124fe"
    "org889a8f2"
    "orgd09529d"
    "org72a9cb4"
    "fig:org2f71c00"
    "sec:org577682a"
    "sec:org16f816a"
    "sec:orgb25c76e"
    "orgb5455b6"
    "sec:org7b8838c"
    "sec:org28b5403"
    "sec:org9371c0f"
    "exp1"
    "exp2"
    "sec:orga62083c"
    "orgaa3f855"
    "sec:org5b1072b"
    "sec:org8718a60"
    "org469b4d6"
    "tab:org5cf6def"
    "org19be2d1"
    "fig:orgbc8ceb4"
    "sec:org82df0c5"
    "org1e3ed66"
    "org0a5a4ee"
    "org7695401"
    "orge0f142a"
    "orgf2506c6"
    "orge990e83"
    "org200144a"
    "fig:org54b906e"
    "sec:org456bcd3"
    "sec:orga4092e5"
    "sec:org1c13b30"
    "sec:orgff928f4"
    "sec:org6b4f9b5"
    "orgbc3815c"
    "fig:org523e75a"
    "sec:orgaf89db0"
    "sec:org4c354d5"
    "sec:orgc174a83"
    "sec:orgb378527"
    "sec:org9116196"
    "sec:org32ecb94"
    "sec:orgcd93f11"
    "sec:org4a6cb1a"
    "sec:org7edf61a"
    "sec:org46ae5e6"
    "sec:org7de1a69"
    "sec:orgb39d760"
    "sec:org8810b40"
    "sec:org73d522d"
    "sec:orgaf7bf29"
    "fig:org412dc19"
    "fig:org9f1e565"
    "fig:orgd058160"
    "orgf1b0f83"
    "sec:org94fe7ac"
    "orgfe4e1ed"
    "orgbdec600")
   (LaTeX-add-bibliographies
    "../../../../Studies/Papers/references"))
 :latex)

