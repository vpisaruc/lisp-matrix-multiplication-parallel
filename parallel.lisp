;; Import packages
(ql:quickload :array-operations)
(ql:quickload :lparallel)

;; Create the worker threads
(setf lparallel:*kernel* (lparallel:make-kernel 4))

;; Create a matrix with random values
(defun InitializeMatrix (rows columns) 
    "Creates a matrix of size rows x columns"
   
    (aops:generate (lambda () (random 10)) (list rows columns))
)

(defun MultiplyRow (matrix1RowIndex matrix2ColumnIndex) 
    "Multiply the matrix1 row at the specified index by 
     the matrix2 column at the specified index"

    (setq sum 0)


    (loop for i from 0 below (length (aops:sub matrix1 matrix1RowIndex))
        do (setf sum  (+ sum (* (aref (aops:sub matrix1 matrix1RowIndex) i) (aref (aops:sub (list-to-2d-array (rotate (2d-array-to-list matrix2))) matrix2ColumnIndex) i)))))

    (setf (aref newMatrix matrix1RowIndex matrix2ColumnIndex) sum)

)

;; Multiply two matrices together
(defun MultiplyMatrices ()
    "Multiplies two matrices"

    ;; Get the dimensions of matrix 1
    (destructuring-bind (n m) (array-dimensions matrix1)
        (setq matrix1Rows n)
        (setq matrix1Columns m)
    )

    ;; Get the dimensions of matrix 2
    (destructuring-bind (n m) (array-dimensions matrix2)
        (setq matrix2Columns m)
        (setq matrix2Rows n)
    )

    ;; Determine dimensions of new matrix
    (defvar newMatrixRows matrix1Rows)
    (defvar newMatrixColumns matrix2Columns)

    (if (/= matrix2Rows matrix1Columns)
        (return-from MultiplyMatrices (PRINT "Столбцы первой матрицы должны быть равны строкам второй матрицы!")))

    ;; Create an empty new matrix will with nil values
    (defvar newMatrix (aops:generate (lambda () ()) (list newMatrixRows newMatrixColumns)))

    ;; Loop through ever row and multiply
    (time (lparallel:pdotimes (i matrix1Rows)
        (dotimes (j matrix2Columns)
            (MultiplyRow i j))))

    (PRINT "Result matrix:")
    (PRINT newMatrix)

)

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists)
)

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j)
                )
    )
)

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list)
)

;; Create two globally scoped matrices
(defvar matrix1 (InitializeMatrix 4 4))
(defvar matrix2 (InitializeMatrix 4 4))
(defvar cnt 0)

(PRINT "Matrix1:")
(PRINT matrix1)

(PRINT "Matrix2:")
(PRINT matrix2)

;; Multiply two matricies in parallel
(MultiplyMatrices)

(EXIT)