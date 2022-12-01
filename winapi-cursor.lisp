(ql:quickload :cffi)
#-win32 (error "Not supported on this platform")

(progn
  (cffi:define-foreign-library user32
    (:windows "user32.dll"))
  (cffi:use-foreign-library user32))


;;; Define the Windows API SetCursorPos function
(cffi:defcfun ("SetCursorPos" %set-cursor-pos) (:boolean :int)
  (x :int)
  (y :int))

(defun set-cursor-pos (pos)
  (check-type (car pos) integer)
  (check-type (cdr pos) integer)
  #+win32 (%set-cursor-pos (car pos) (cdr pos)))


;;; Define the Windows API GetCursorPos function
(cffi:defcfun (%get-cursor-pos "GetCursorPos") (:boolean :int)
  (lp-point :pointer))

(cffi:defcstruct point
  (x :long)
  (y :long))

(defun get-cursor-pos ()
  (cffi:with-foreign-object (p '(:struct point))
    (%get-cursor-pos p)
    (cons (cffi:foreign-slot-value p '(:struct point) 'x)
          (cffi:foreign-slot-value p '(:struct point) 'y))))


;;; Define the Windows API SendInput function
(cffi:defctype word :unsigned-short)
(cffi:defctype dword :unsigned-long)
(cffi:defctype ulong-ptr #+:x86-64 :uint64 #-:x86-64 :unsigned-long)

(cffi:defcstruct tag-mouse-input
  (dx :long)
  (dy :long)
  (mouse-data dword)
  (dw-flags dword)
  (time dword)
  (dw-extra-info ulong-ptr))

(defconstant +xbutton1+ #x0001)
(defconstant +xbutton2+ #x0002)
(defconstant +mouseeventf-move+ #x0001)
(defconstant +mouseeventf-leftdown+ #x0002)
(defconstant +mouseeventf-leftup+ #x0004)
(defconstant +mouseeventf-rightdown+ #x0008)
(defconstant +mouseeventf-rightup+ #x0010)
(defconstant +mouseeventf-middledown+ #x0020)
(defconstant +mouseeventf-middleup+ #x0040)
(defconstant +mouseeventf-xdown+ #x0080)
(defconstant +mouseeventf-xup+ #x0100)
(defconstant +mouseeventf-wheel+ #x0800)
(defconstant +mouseeventf-hwheel+ #x1000)
(defconstant +mouseeventf-virtualdesk+ #x4000)
(defconstant +mouseeventf-absolute+ #x8000)
(defconstant +wheel-delta+ 120 "The amount of mouse wheel movement for one wheel click.")

(cffi:defcstruct tag-keybd-input
  (w-vk word)
  (w-scan word)
  (dw-flags dword)
  (time dword)
  (dw-extra-info ulong-ptr))

(defconstant +keyeventf-extendedkey+ #x0001)
(defconstant +keyeventf-keyup+ #x0002)
(defconstant +keyeventf-scancode+ #x0008)
(defconstant +keyeventf-unicode+ #x0004)

(cffi:defcstruct tag-hardware-input
  (u-msg dword)
  (w-param-l word)
  (w-param-h word))

(cffi:defctype mi (:struct tag-mouse-input))
(cffi:defctype ki (:struct tag-keybd-input))
(cffi:defctype hi (:struct tag-hardware-input))

(cffi:defcunion input-union
  (mi mi)
  (ki ki)
  (hi hi))

(cffi:defcstruct tag-input
  (type dword)
  (dummy-union-name (:union input-union)))

(cffi:defctype input (:struct tag-input))

(defconstant +input-mouse+ 0)
(defconstant +input-keyboard+ 1)
(defconstant +input-hardware+ 2)

(cffi:defcfun (%send-input "SendInput" :convention :stdcall) :uint
  "Send keyboard or mouse events and return the number of events sent successfully."
  (c-inputs :uint)
  (p-inputs :pointer)
  (cb-size :int))

(defstruct mouse-input
  "A Lisp version of the tag-mouse-input CFFI structure."
  (dx 0)
  (dy 0)
  (mouse-data 0)
  (dw-flags 0)
  (time 0)
  (dw-extra-info 0))

(defstruct keybd-input
  "A Lisp version of the tag-keybd-input CFFI structure."
  (w-vk 0)
  (w-scan 0)
  (dw-flags 0)
  (time 0)
  (dw-extra-info 0))

(defun send-inputs (inputs)
  "Call the Windows API SendInput function with a list of mouse-input/keybd-input structs."
  (let ((num-inputs (length inputs)))
    (cffi:with-foreign-object (in 'input num-inputs)
      (loop for i from 0 below num-inputs
            for input in inputs
            for input-ptr = (cffi:mem-aref in 'input i)
            for union-ptr = (cffi:foreign-slot-pointer input-ptr 'input 'dummy-union-name)
            do (etypecase input
                 (mouse-input
                  (setf (cffi:foreign-slot-value input-ptr 'input 'type) +input-mouse+
                        (cffi:foreign-slot-value union-ptr 'mi 'dx) (mouse-input-dx input)
                        (cffi:foreign-slot-value union-ptr 'mi 'dy) (mouse-input-dy input)
                        (cffi:foreign-slot-value union-ptr 'mi 'mouse-data) (mouse-input-mouse-data input)
                        (cffi:foreign-slot-value union-ptr 'mi 'dw-flags) (mouse-input-dw-flags input)
                        (cffi:foreign-slot-value union-ptr 'mi 'time) (mouse-input-time input)
                        (cffi:foreign-slot-value union-ptr 'mi 'dw-extra-info) (mouse-input-dw-extra-info input)))
                 (keybd-input
                  (setf (cffi:foreign-slot-value input-ptr 'input 'type) +input-keyboard+
                        (cffi:foreign-slot-value union-ptr 'ki 'w-vk) (keybd-input-w-vk input)
                        (cffi:foreign-slot-value union-ptr 'ki 'w-scan) (keybd-input-w-scan input)
                        (cffi:foreign-slot-value union-ptr 'ki 'dw-flags) (keybd-input-dw-flags input)
                        (cffi:foreign-slot-value union-ptr 'ki 'time) (keybd-input-time input)
                        (cffi:foreign-slot-value union-ptr 'ki 'dw-extra-info) (keybd-input-dw-extra-info input)))))
      (= num-inputs (%send-input num-inputs in (cffi:foreign-type-size 'input))))))
