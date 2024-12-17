;création du neurone
(setq *neurone* '((t 0) (poids ((w0 1) (w1 1) (w2 1) (w3 1) (w4 1) (w5 1) (w6 1) (w7 1))) (sortie 0)))

;initialisation des valeurs de x
(setq numeros 
      '((0 ((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 0)))
        (1 ((x0 1) (x1 0) (x2 1) (x3 1) (x4 0) (x5 0) (x6 0) (x7 0)))
        (2 ((x0 1) (x1 1) (x2 1) (x3 0) (x4 1) (x5 1) (x6 0) (x7 1)))
        (3 ((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 0) (x6 0) (x7 1)))
        (4 ((x0 1) (x1 0) (x2 1) (x3 1) (x4 0) (x5 0) (x6 1) (x7 1)))
        (5 ((x0 1) (x1 1) (x2 0) (x3 1) (x4 1) (x5 0) (x6 1) (x7 1)))
        (6 ((x0 1) (x1 0) (x2 0) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (7 ((x0 1) (x1 1) (x2 1) (x3 1) (x4 0) (x5 0) (x6 0) (x7 0)))
        (8 ((x0 1) (x1 1) (x2 1) (x3 1) (x4 1) (x5 1) (x6 1) (x7 1)))
        (9 ((x0 1) (x1 1) (x2 1) (x3 1) (x4 0) (x5 0) (x6 1) (x7 1)))
        )
  )

(defun processNeurone (neurone list_of_values number)
  (let* ((num (assoc number list_of_values)); Pour recuper les bons inputs
         (weights (cadr (assoc 'poids neurone))); Pour recuperer une alist
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

(processNeurone *neurone* numeros 0)

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
         (weightsT (cadr(assoc 'poids neurone)))
         (inputsT (cadr num))
         (c (cadr(assoc 'sortie neurone)))
         (o (sortie_calculee somme))
         (weightsT_1 ()))
    
    (loop for weight in weightsT for input in inputsT do
          (setq weightsT_1 (append weightsT_1 (list (list (car weight) ( +(* (- c o) (cadr input)) (cadr weight)))))))
          
          ;(print weightsT_1)
    (list (list 't (+ (cadr (assoc 't neurone)) 1)) (list 'poids weightsT_1) (list 'sortie (sortie_attendue (+ (cadr(assoc 't neurone)) 1))))
         
      )
    )

(defun apprentissage_avec_correction_erreur (echantillon)
  (let ((size_e (- (length echantillon) 1))
         (ok NIL)
         (neurone '((t 0) (poids ((w0 1) (w1 1) (w2 1) (w3 1) (w4 1) (w5 1) (w6 1) (w7 1))) (sortie 0))))
    
    (while (not ok)
      (setq ok T)
      (loop for i from 0 to size_e do
            ;(print i)
            (let* ((somme (processNeurone neurone echantillon i))
                   (c (cadr(assoc 'sortie neurone)))
                   (o (sortie_calculee somme)))
              
            (if (not (eq c o))
                (progn
                  (setq neurone (correction_erreur neurone echantillon i))
                  ;(print neurone)
                  (setq ok (and ok NIL))
                  )
              )
              )
            )
      )
    neurone
    )
  )

(apprentissage_avec_correction_erreur numeros)