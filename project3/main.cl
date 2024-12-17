;création du neurone
(setf neurone (list (list '0 '((w0 1) (w1 1) (w2 1) 
                                (w3 1) (w4 1) (w5 1) 
                                (w6 1) (w7 1)))))

;initialisation des valeurs de x
(setf numeros 
  (list (list '0 '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 0)))
        (list '1 '((x0 1) (x1 0) (x2 1) (x3 1) (x4 0) (x5 0) (x6 0) (x7 0)))
        (list '2 '((x0 1) (x1 1) (x2 1) (x3 0) (x4 1) (x5 1) (x6 0) (x7 1)))
        (list '3 '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 0) (x6 0) (x7 1)))
        (list '4 '((x0 1) (x1 0) (x2 1) (x3 1) (x4 0) (x5 0) (x6 1) (x7 1)))
        (list '5 '((x0 1) (x1 1) (x2 0) (x3 1) (x4 1) (x5 0) (x6 1) (x7 1)))
        (list '6 '((x0 1) (x1 1) (x2 0) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (list '7 '((x0 1) (x1 1) (x2 1) (x3 1) (x4 0) (x5 0) (x6 0) (x7 0)))
        (list '8 '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (list '9 '((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 0) (x6 1) (x7 1)))
        )
  )

(defun processNeurone (neurone list_of_values number)
  (let* ((num (assoc number list_of_values)); Pour recuper les bons inputs
         (weights (cadr (assoc number neurone))); Pour recuperer une alist
         (inputs (cadr num))
         (result 0)); initialisation a 0
    
    ;(print num)
    ;(print weights)
    ;(print inputs)
    (loop for weight in weights
          for input in inputs
          do (setf result (+ result
                             (* (cadr weight)
                                (cadr input))))) 
          
    result
    )
  )

(processNeurone neurone numeros 0)

(defun sortie_attendue (number)
  (if (eq (rem number 2) 1)
      1
      0)
  )

(sortie_attendue 0)


(defun sortie_calculee (somme)
  (if (> somme 0)
      1
      0)
  )

(sortie_calculee 7)

(defun correction_erreur (neurone list_of_values number)
  (let* ((num (assoc number list_of_values))
         (somme (processNeurone neurone list_of_values number))
         (weightsT (cadr (assoc number neurone)))
         (inputsT (cadr num))
         (c (sortie_attendue number))
         (o (sortie_calculee somme))
         (weightsT_1 (list (+ number 1))))
    
    (if (not (eq c o))
        (progn
          (loop for weight in weightsT
              for input in inputsT
              do (nconc weightsT_1 (list (list (car weight) ( +(* (- c o) (cadr input)) (cadr weight)))))
                )
          ;(print weightsT_1)
          (nconc neurone (list weightsT_1))
          ;(print neurone)
          )
      )
    )
  )

(correction_erreur neurone numeros 0)
neurone
