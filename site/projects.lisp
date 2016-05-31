`(:title "Projects"
  :metadata (:published "2015-08-28"
             :updated "2016-02-18")
  :content-block
         ,(flet ((project (name link description)
                   `((:dt (:a :href ,link ,name))
                     (:dd ,description))))
            `((:p "A somewhat comprehensive list of my publicly maintained
                   projects for personal reference.  Most of these are
                   presentable (stable, functioning, usable, etc.) so if you
                   find them useful, feel free to use them.  Contact me if you
                   need a free software license and one isn't present.")
              (:dl
               ,@(project
                  "linca"
                  "https://github.com/darkfeline/linca"
                  "A simple program for watching directories and linking new
                  files.")))))
