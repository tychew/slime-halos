
(ql:quickload "alexandria")

(in-package :swank-halos)

(defun temp-jpg ()
  (make-instance 'presentation-image
                 :data (alexandria:read-file-into-byte-vector
                        (load-time-value (merge-pathnames "test.jpg" *load-pathname*)))
                 :format "jpeg"))

(defun temp-png ()
  (make-instance 'presentation-image
                 :data (alexandria:read-file-into-byte-vector
                        (load-time-value (merge-pathnames "test.png" *load-pathname*)))
                 :format "png"))
