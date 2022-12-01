(load "c:/Users/kcwch/Desktop/macro/winapi-cursor.lisp")

(defvar *key-pos-map* (make-hash-table))

(defun list-map ()
  (flet ((print-mapping (key value) (format t "key ~S is mapped to ~S~%" key value)))
    (maphash #'print-mapping *key-pos-map*)))

(defun set-current-pos (key)
  (setf (gethash key *key-pos-map*) (get-cursor-pos)))


(defun goto-key (key)
  (let ((pos (gethash key *key-pos-map*)))
    (if pos
        (set-cursor-pos pos)
        (error "key ~S is not mapped" key))))

(defparameter click-duration 0.02)

(defmacro left-click ()
  `(progn
     (send-inputs (list (make-mouse-input :dw-flags +mouseeventf-leftdown+)))
     (time (sleep click-duration))
     (send-inputs (list (make-mouse-input :dw-flags +mouseeventf-leftup+)))))

(defmacro right-click ()
  `(progn
     (send-inputs (list (make-mouse-input :dw-flags +mouseeventf-rightdown+)))
     (time (sleep click-duration))
     (send-inputs (list (make-mouse-input :dw-flags +mouseeventf-rightup+)))))

(defmacro left-click-key (key)
  `(progn
     (goto-key ,key)
     (left-click)))

(defun attack-1 ()
  (progn
    (left-click-key 'key-1)
    (left-click-key 'key-2)
    (left-click-key 'key-3)
    (left-click-key 'key-4)))

(defun attack-2 ()
  (loop
    (progn
      (left-click-key 'key-1)
      (left-click-key 'key-2)
      (left-click-key 'key-3)
      (left-click-key 'key-4)
      (left-click-key 'key-5)
      (left-click-key 'key-6)
      (left-click-key 'key-7)
      (left-click-key 'key-8)
      (left-click-key 'key-9))))

(defun macro-attack ()
  (loop
    (attack-1)
    (time (sleep 4))))

(defun macro-stop ())
