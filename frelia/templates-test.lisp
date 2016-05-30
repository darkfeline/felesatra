(in-package :frelia-templates-test)

(plan 1)
(is (html-base-template :title "Title"
                        :head-block '("Head")
                        :body-block '("Body"))
    '(:html :lang "en"
      (:head
       (:meta :charset "UTF-8")
       (:title "Title")
       "Head")
      (:body "Body")))
(finalize)
