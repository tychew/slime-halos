
(defpackage :swank-halos
  (:use :cl)
  (:import-from #:swank
                #:menu-choices-for-presentation
                #:lookup-presented-object-or-lose
                #:defslimefun)
  (:export #:presentation-image
           #:make-halo-dispatch))

(in-package :swank-halos)

(defconstant +placeholder+
  #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 80 0 0 0 30 8 6 0 0 0 187 67 153 177 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 194 0 0 14 194 1 21 40 74 128 0 0 2 127 73 68 65 84 104 67 237 152 59 110 227 48 20 69 175 102 45 210 12 96 120 5 244 10 100 55 174 220 166 163 202 168 153 46 192 52 233 220 72 165 221 165 117 149 198 210 10 162 21 24 41 70 222 139 230 61 145 250 121 228 128 49 157 17 6 224 1 136 152 143 31 61 93 93 126 16 175 34 224 184 153 111 250 175 227 70 188 234 215 15 231 64 11 156 3 45 113 2 90 226 4 180 196 9 104 137 187 198 88 226 28 104 137 19 208 18 39 160 37 35 2 230 136 60 15 94 191 68 185 110 179 229 140 116 177 64 122 214 85 67 242 232 158 57 220 151 43 14 20 72 202 10 124 190 212 101 23 234 248 52 132 187 233 115 184 198 39 150 48 57 115 145 34 101 55 212 206 140 40 210 185 117 209 218 138 99 17 210 116 209 58 88 181 177 251 2 196 69 129 56 160 184 158 171 27 71 61 120 204 136 211 56 62 152 223 56 143 166 15 151 161 243 107 87 247 219 105 206 58 203 94 222 70 174 231 107 204 144 172 146 20 230 38 85 68 69 110 108 227 66 85 170 76 246 218 202 164 18 144 212 163 110 25 244 83 245 102 142 178 74 68 243 155 200 100 5 145 80 148 185 104 235 81 38 226 98 62 147 60 46 232 63 139 251 181 207 237 229 55 136 171 185 229 232 100 29 6 75 248 13 143 190 14 67 226 73 87 130 153 160 234 147 106 243 87 216 136 19 126 183 95 184 235 71 11 16 107 89 224 189 212 213 62 225 26 178 56 224 88 127 250 35 14 216 96 213 62 235 35 204 242 24 184 105 185 87 193 15 56 31 15 40 138 24 129 30 195 67 78 221 75 141 50 241 41 28 226 103 2 28 72 65 78 30 155 21 140 244 51 225 156 226 33 158 131 12 164 140 80 38 100 11 141 255 29 243 86 168 37 78 201 75 103 18 153 105 227 168 242 214 54 140 243 69 2 238 241 220 108 56 244 34 207 123 129 89 160 170 151 248 171 13 41 184 197 182 214 239 110 242 1 229 59 10 49 67 243 216 124 27 163 208 191 145 191 146 104 229 95 34 113 46 98 255 74 187 167 57 87 4 212 27 125 99 255 79 95 33 36 45 198 7 53 54 136 49 207 154 109 192 7 235 213 28 34 181 196 188 236 72 240 189 241 242 53 36 220 33 155 247 150 35 229 68 123 167 34 152 113 18 221 251 81 169 95 209 127 196 75 114 194 178 141 27 92 185 232 11 220 25 222 148 175 108 228 87 48 217 172 239 9 63 175 59 148 8 62 96 110 76 96 226 61 144 168 151 184 196 250 31 94 243 66 222 120 251 14 92 2 217 141 247 76 247 223 24 75 166 119 224 127 142 19 208 18 143 138 91 194 22 56 7 90 226 4 180 196 9 104 137 19 208 18 119 15 180 2 248 3 99 209 6 252 18 26 252 255 0 0 0 0 73 69 78 68 174 66 96 130)
  "This was read in using (alexandria:read-file-into-byte-vector \"empty.png\")")

(defclass presentation-image ()
  ((data :accessor data-of
         :initarg :data
         :initform +placeholder+)
   (format :accessor format-of
           :initarg :format
           :initform "png")))

(defun image-data-for-emacs (image)
  (list (coerce (data-of image) 'list)
        (format-of image)))

(defmethod menu-choices-for-presentation ((ob presentation-image))
  (cons (list "Something"
              (lambda (choice object id)
                (declare (ignore choice object id))
                nil))
        (when (next-method-p) (call-next-method))))

(defun make-halo-dispatch (id)
  (let ((value (lookup-presented-object-or-lose id)))
    `(:presentation-insert-image
      ,id
      ,(image-data-for-emacs value)
      ,(prin1-to-string value)
      :repl-result)))

(in-package :swank)

;; Patching the original in swank-presentation
(defun present-repl-results (values)
  ;; Override a function in swank.lisp, so that
  ;; presentations are associated with every REPL result.
  (flet ((send (value)
           (let ((id (and *record-repl-results*
                          (save-presented-object value))))
             (if (typep value 'swank-halos:presentation-image)
                 (progn
                   (send-to-emacs (swank-halos:make-halo-dispatch id))
                   (send-to-emacs `(:write-string ,(string #\Newline) :repl-result)))
                 (progn
                   (send-to-emacs `(:presentation-start ,id :repl-result))
                   (send-to-emacs `(:write-string ,(prin1-to-string value) :repl-result))
                   (send-to-emacs `(:presentation-end ,id :repl-result))
                   (send-to-emacs `(:write-string ,(string #\Newline) :repl-result)))))))
    (fresh-line)
    (finish-output)
    (if (null values)
        (send-to-emacs `(:write-string "; No value" :repl-result))
        (mapc #'send values))))

;; Patched to add our events to the whitelist. From swank.lisp
(defun dispatch-event (connection event)
  "Handle an event triggered either by Emacs or within Lisp."
  (log-event "dispatch-event: ~s~%" event)
  (dcase event
    ((:emacs-rex form package thread-id id)
     (let ((thread (thread-for-evaluation connection thread-id)))
       (cond (thread
              (add-active-thread connection thread)
              (send-event thread `(:emacs-rex ,form ,package ,id)))
             (t
              (encode-message 
               (list :invalid-rpc id
                     (format nil "Thread not found: ~s" thread-id))
               (current-socket-io))))))
    ((:return thread &rest args)
     (remove-active-thread connection thread)
     (encode-message `(:return ,@args) (current-socket-io)))
    ((:emacs-interrupt thread-id)
     (interrupt-worker-thread connection thread-id))
    (((:write-string 
       :debug :debug-condition :debug-activate :debug-return :channel-send
       :presentation-start :presentation-end
       :new-package :new-features :ed :indentation-update
       :eval :eval-no-wait :background-message :inspect :ping
       :y-or-n-p :read-from-minibuffer :read-string :read-aborted :test-delay
       :presentation-insert-image)
      &rest _)
     (declare (ignore _))
     (encode-message event (current-socket-io)))
    (((:emacs-pong :emacs-return :emacs-return-string) thread-id &rest args)
     (send-event (find-thread thread-id) (cons (car event) args)))
    ((:emacs-channel-send channel-id msg)
     (let ((ch (find-channel channel-id)))
       (send-event (channel-thread ch) `(:emacs-channel-send ,ch ,msg))))
    ((:reader-error packet condition)
     (encode-message `(:reader-error ,packet 
                                     ,(safe-condition-message condition))
                     (current-socket-io)))))

;; ========================================
;; For slime-eval-present-last-sexp

(defslimefun eval-and-grab-presentations (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME evaluation request.")
      (let* ((s (make-string-output-stream))
             (*standard-output* s)
             (values (multiple-value-list (eval (from-string string)))))
        (list (get-output-stream-string s)
              (mapcar (lambda (v)
                        (list (format nil "~S" v)
                              (save-presented-object v)))
                      values))))))

(provide :swank-halos)
