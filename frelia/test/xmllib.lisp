(in-package :frelia-xmllib-test)

(plan 2)
(is (format-attrs '(("foo" . "bar")))
    '("foo=\"bar\""))
(is (format-attrs '(("Mir" . "Jakuri")
                    ("Shurelia" . "Eoria")))
    '("Mir=\"Jakuri\""
      "Shurelia=\"Eoria\""))
(finalize)

(plan 1)
(is (render-content 'reverse '("foo" (a b) "bar"))
    '("foo" (b a) "bar"))
(finalize)
