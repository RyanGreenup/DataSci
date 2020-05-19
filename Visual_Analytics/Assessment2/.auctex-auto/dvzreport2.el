(TeX-add-style-hook
 "dvzreport2"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
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
    "sec:orga05d9d0"
    "sec:org52451a6"
    "sec:org1cf5590"
    "sec:orgab0a512"
    "sec:orga58e60f"
    "sec:org0b00a1e"
    "sec:org656195d"
    "sec:org659db8c"
    "sec:orgc5e9a94"
    "sec:orga71b4b8"
    "sec:org2387bb0"
    "orga9c42ef"
    "org26e6ca5"
    "orgd95fccc"
    "org0d2cbb1"
    "orgf44bd0b"
    "fig:org186877f"
    "sec:org02053d1"
    "org05eca33"
    "org11df32b"
    "org9e9e29a"
    "fig:org819d68a"
    "sec:org416bd07"
    "sec:org21a3513"
    "sec:orgcababeb"
    "sec:orge275014"
    "sec:org782b77f"
    "sec:org84f8ded"
    "sec:org342f127"
    "sec:org910e724"
    "sec:org2b0c0e7"
    "sec:org89448c5"
    "sec:orgc22965a"
    "sec:orgf9a369f"
    "sec:org9ac1292"
    "sec:org434bc6c"
    "sec:orgf2078a4"
    "sec:orged132ee"
    "sec:org12d11b8"
    "sec:org0d9140c"
    "sec:org992d220"
    "sec:orgbc36f7f"
    "sec:org2de2d93"
    "sec:org05f63f6"
    "sec:orgdcaf953"
    "sec:orged456ea"
    "org98acb60"
    "org0b1333d")
   (LaTeX-add-bibliographies
    "../../../../Studies/Papers/references"))
 :latex)

