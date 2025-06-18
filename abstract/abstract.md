---
title: Automatiser la vérification d’expressions algébriques
subtitle: \textit{simplification symbolique et preuves par réécriture}
author: Victor ROBERT
date: Juin 2025
documentclass: extarticle
references:
  - id: patternopt
    author:
        - family: BALLAND
          given: Emilie
        - family: MOREAU
          given: Pierre-Etienne
    title: Optimizing Pattern Matching by Program Transformation
    URL: "https://inria.hal.science/inria-00001127/document"
    type: article-journal

  - id: coqencoq
    author:
        - family: BARRAS
          given: Bruno
    issued:
        - year: 1996
        - month: 10
    URL: "https://inria.hal.science/inria-00073667/document"
    type: article-journal
    title: Coq en coq
  - id: cwinl
    author:
        - family: KLOP
          given: Jan Willem
    issued:
        - year: 2000
        - month: 8
    URL: "https://ir.cwi.nl/pub/6129/6129D.pdf"
    type: article-journal

  - id: lmaranget
    author:
        - family: MARANGET
          given: Luc
    issued:
        - year: 2008
        - month: 6
        - day: 16
    URL: "https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf"
    type: article-journal

  - id: sometapas
    author:
        - family: MESEGUER
          given: José
    URL: "https://maude.cs.illinois.edu/w/images/7/70/Maude-tapas.pdf"
    type: article-journal

  - id: lambda
    author:
        family: CRABBE
        given: Marcel
    issued:
        - year: 1986
    URL: "https://www.cahiersdelogique.be/Copies/CCL6/CCL6-crabbe.PDF"
    type: article-journal

nocite: |
    @coqencoq, @sometapas, @lmaranget, @cwinl, @patternopt, @lambda
---

# \boxed{\textbf{Abstract}}

Dans ce TIPE, on s'intéresse aux liens entre le raisonnement en mathématiques et la programmation informatique. En effet, en informatique, on ne fais pas vraiment de manipulations abstraites comme en mathématiques. Chaque variable doit avoir une valeur définie en mémoire pour être interprétée. On pourra alors étudier les moyens possibles pour simuler un raisonnement mathématique faisant intervenir des objets abstraits. Pour ce faire on utilise un interpréteur en OCaml ([Trouvable ici](https://github.com/lhitori/uXm)) qui implémente un _langage dédié_ (ou [DSL](https://en.wikipedia.org/wiki/Domain-specific_language)) le plus simpliste possible. Celui-ci permettant de faire des transformations mathématiques diverses, on peut néanmoins s'appuyer sur des preuves de correction pour être sûr de conserver une certaine rigueur. Une idée serait alors d'arriver à prouver le prouveur dans sa propre syntaxe, de sorte qu'on arrive à un système complètement autonome.


# References

