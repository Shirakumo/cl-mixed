## Testing new drains and sources
In order to test a new drain or source quickly, you can use the following function:

```lisp
(defun play (source drain &rest drain-args)
  (org.shirakumo.fraf.mixed:with-objects
      ((drain (apply #'make-instance drain (append drain-args (list :channels 2))))
       (source (etypecase source
                 (mixed:source (setf (mixed:pack source) (mixed:pack drain)) source)
                 (pathname (make-instance 'org.shirakumo.fraf.mixed.mpg123:source :file source :pack (mixed:pack drain)))
                 (symbol (make-instance source :pack (mixed:pack drain))))))
    (org.shirakumo.fraf.mixed:with-chain chain (source drain)
      (format T "~&Playing back on ~d channels @ ~dHz~%"
              (org.shirakumo.fraf.mixed:channels drain) (org.shirakumo.fraf.mixed:samplerate drain))
      (loop (org.shirakumo.fraf.mixed:mix chain)))))
```
