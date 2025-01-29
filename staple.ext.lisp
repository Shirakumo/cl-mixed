(defmethod staple:subsystems ((system (eql (asdf:find-system :cl-mixed)))) ())
(defmethod staple:images ((system (eql (asdf:find-system :cl-mixed)))) ())
(defmethod staple:documents ((system (eql (asdf:find-system :cl-mixed))))
  (list (asdf:system-relative-pathname system #p"README.mess")))
