(defun csv-to-hash (file-path table key)
  (with-open-file (stream file-path :direction :input)

    (read-line stream)

    (cond
      ((eq key :projects)
       (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
           ((eq line 'eof))
         (let* ((columns (uiop:split-string line :separator ","))
                (proj-id (parse-integer (first columns)))
                (proj-name (string-trim '(#\Space #\Tab #\Newline #\Return) (second columns)))
                (model (string-trim '(#\Space #\Tab #\Newline #\Return) (third columns)))
                (nested-table (make-hash-table)))
           (setf (gethash :project-id nested-table) proj-id)
           (setf (gethash :project-name nested-table) proj-name)
           (setf (gethash :model-name nested-table) model)
           (setf (gethash proj-id table) nested-table))))
      
      ((eq key :models)
       (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
           ((eq line 'eof))
         (let* ((columns (uiop:split-string line :separator ","))
                (model-id (parse-integer (first columns)))
                (model-name (string-trim '(#\Space #\Tab #\Newline #\Return) (second columns)))
                (nested-table (make-hash-table)))
           (setf (gethash :model-id nested-table) model-id)
           (setf (gethash :model-name nested-table) model-name)
           (setf (gethash model-id table) nested-table))))

      (t (format t "Unknown key: ~A~%" key)))))

(defun select (file-path key &rest filters)
  (lambda (&rest filters)
    (let ((output '())
          (table (make-hash-table :test #'equal)))

      (csv-to-hash file-path table key)

      (if (null filters)
          (maphash (lambda (k v)
                     (push v output))
                   table)
          
          (let* ((filter-map (make-hash-table :test #'equal)))
            (loop for (filter-k filter-v) on filters by #'cddr
                  do (setf (gethash filter-k filter-map) filter-v))

            (maphash (lambda (k v)
                       (let ((nested-table v)
                             (match t))
                         (maphash (lambda (filter-k filter-v)
                                    (let ((nested-val (gethash filter-k nested-table)))
                                      (when (not (equal filter-v nested-val))
                                        (setf match nil))))
                                  filter-map)
                         (when match
                           (push nested-table output))))
                     table)))
      (reverse output))))

(defun add-data (file-path tables)
  (with-open-file (stream file-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let* ((keys (loop for k being the hash-keys of (first tables) collect k))
           (header (mapcar #'string keys)))
      (format stream "~{~a~^,~}~%" header)
      (dolist (table tables)
        (let ((values (mapcar (lambda (k) 
                                (let ((val (gethash k table)))
                                  (if val 
                                      (write-to-string val)
                                      "")))
                              keys)))
          (format stream "~{~a~^,~}~%" values))))))

(defun hash-table-to-alist (table)
  (let ((alist '()))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    (nreverse alist)))

(defun output-data (tables)
  (let ((fields (let ((keys '()))
                  (maphash (lambda (k v)
                             (push k keys)) (first tables))
                  (reverse keys))))
    (format t "~%")
    (format t "~{~20A~}" (mapcar #'symbol-name fields))
    (format t "~%")
    (dolist (table tables)
      (let ((values (mapcar (lambda (k) (gethash k table)) fields)))
        (format t "~{~20A~}" values)
        (format t "~%")))))


(defun testing ()
  (format t "~%SELECT * FROM projects~%")
  (output-data (funcall (select "d:/kpi/k4s1/functional programming/projects.csv" :projects)))

  (format t "~%SELECT * FROM models:~%")
  (output-data (funcall (select "d:/kpi/k4s1/functional programming/aimodels.csv" :models)))

  (format t "~%SELECT * FROM projects WHERE model-name='ai3'~%")
  (output-data (funcall (select "d:/kpi/k4s1/functional programming/projects.csv" :projects) :model-name "ai3"))

  (format t "~%SELECT * FROM projects WHERE project-name='project2'~%")
  (output-data (funcall (select "d:/kpi/k4s1/functional programming/projects.csv" :projects) :project-name "project2"))

  (format t "~%SELECT * FROM models WHERE model-id=1~%")
  (output-data (funcall (select "d:/kpi/k4s1/functional programming/projects.csv" :models) :model-id 1))
  
  (format t "~%INSERT INTO projects VALUES~%")
  (output-data (funcall (select "d:/kpi/k4s1/functional programming/projects.csv" :projects) :model-name "ai2"))
  
  (add-data "d:/kpi/k4s1/functional programming/output.csv" (funcall (select "d:/kpi/k4s1/functional programming/projects.csv" :projects) :model-name "ai2"))
  
  (with-open-file (stream "d:/kpi/k4s1/functional programming/output.csv" :direction :input)
    (format t "~%output.csv:~%")
    (loop for line = (read-line stream nil)
          while line
          do (format t "~a~%" line)))
  (let* ((table (make-hash-table :test 'equal))
         (expected-alist '((:project-id . 1)
                           (:project-name . "project1")
                           (:model-name . "ai1"))))

    (setf (gethash :project-id table) 1)
    (setf (gethash :project-name table) "project1")
    (setf (gethash :model-name table) "ai1")

    (let ((generated-alist (hash-table-to-alist table)))
      (if (equal expected-alist generated-alist)
          (format t "Passed test for alist: ~a~%" generated-alist)
          (format t "Failed test for alist: ~a~%" generated-alist)))))


(testing)
