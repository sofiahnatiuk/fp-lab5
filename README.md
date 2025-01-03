<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Гнатюк Софія Валентинівна КВ-12</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
- структури у геш-таблиці
- геш-таблиці у асоціативні списки
- асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
## Варіант <5>
База даних: Проєкти із застосуванням ШІ, геш-таблиця
## Лістинг реалізації завдання
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
SELECT * FROM projects
PROJECT-ID          PROJECT-NAME        MODEL-NAME          
1                   project1            ai1                 
2                   project2            ai1                 
3                   project3            ai2                 
4                   project4            ai2                 
5                   project5            ai3                 

SELECT * FROM models:
MODEL-ID            MODEL-NAME          
1                   ai1                 
2                   ai2                 
3                   ai3                 

SELECT * FROM projects WHERE model-name='ai3'
PROJECT-ID          PROJECT-NAME        MODEL-NAME          
5                   project5            ai3                 

SELECT * FROM projects WHERE project-name='project2'
PROJECT-ID          PROJECT-NAME        MODEL-NAME          
2                   project2            ai1                 

SELECT * FROM models WHERE model-id=1
MODEL-ID            MODEL-NAME          
1                   project1            

INSERT INTO projects VALUES
PROJECT-ID          PROJECT-NAME        MODEL-NAME          
3                   project3            ai2                 
4                   project4            ai2                 

output.csv:
PROJECT-ID,PROJECT-NAME,MODEL-NAME
3,"project3","ai2"
4,"project4","ai2"
Passed ((PROJECT-ID . 1) (PROJECT-NAME . project1) (MODEL-NAME . ai1))
```
