
;création du neurone
(setf neurone (list 'neurone1 '((w0 0) (w1 1) (w2 1) (w3 1) (w4 1) (w5 1) (w6 1) (w7 1))))

;initialisation des valeurs de x
(setf numeros 
  (list (list 'zero '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 0)))
        (list 'un '((x0 1) (x1 0) (x2 1) (x3 1) (x4 0) (x5 0) (x6 0) (x7 0)))
        (list 'deux '((x0 1) (x1 1) (x2 1) (x3 0) (x4 1) (x5 1) (x6 0) (x7 1)))
        (list 'trois '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 0) (x6 0) (x7 1)))
        (list 'quatre '((x0 1) (x1 0) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1))):faux à partir de la regarde ta correction
        (list 'cinq '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (list 'six '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (list 'sept '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (list 'huit '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (list 'neuf '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        )
  )


(processNeurone neurone numeros 'un)

(defun processNeurone (neurone list_of_values number)
  (let* ((num (assoc number list_of_values)); Pour recuper les bons inputs
         (weights (cdr (second neurone))); Pour recuperer une alist
         (inputs (cdr (second num)))
         (result 0)); initialisation a 0
    (loop for weight in weights
          for input in inputs
          do (setf result (+ result
                             (* (cadr weight)
                                (cadr input)))))       
    result))
