(eval-when-compile
  (require 'cl-lib))

(defmacro if-let ( binding then &optional else )
  "Bind value according to BINDING and check for truthy-ness
If the test passes then eval THEN with the BINDING varlist bound
If no, eval ELSE with no binding"
  (let* ((sym (caar binding))
         (tst (cdar binding))
         (gts (cl-gensym)))
    `(let ((,gts ,@tst))
       (if ,gts
         (let ((,sym ,gts))
           ,then)
         ,else))))
