(in-package :frelia-xml-test)

(plan 1)
(is (render-xml-element '(:test)) "<test></test>")
(finalize)
