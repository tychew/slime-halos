
(require 'slime)
(require 'slime-presentations)
(require 'cl-lib)

(define-slime-contrib slime-halos
  "Use presentations to provide functionality for halos (similar to Smalltalk)."
  (:authors "Theam Yong Chew <chew.theam.yong@gmail.com")
  (:license "MIT")
  (:slime-dependencies slime-repl slime-presentations)
  (:swank-dependencies swank-presentations swank-halos)
  (:on-load   (add-hook 'slime-event-hooks 'slime-dispatch-image-presentation-event))
  ;; TODO There are lots of side effects we'll have to undo. How can
  ;; we do this better?
  (:on-unload (remove-hook 'slime-event-hooks 'slime-dispatch-image-presentation-event)))

(defun slime-insert-image-presentation (image output-id)
  "Inserts a halo presentation."
  (let ((start (point)))
    (insert-image image)
    (slime-add-presentation-properties start (point) output-id t)
    (add-text-properties start (point) '(image t))))

(defun slime-insert-image-presentation-stub (string output-id)
  (let ((start (point)))
    (insert string)
    (slime-add-presentation-properties start (point) output-id t)
    (add-text-properties start (point) '(image nil))))

(defun slime-presentation-write-image-result (image id)
  (with-current-buffer (slime-output-buffer)
    (let ((marker (slime-output-target-marker :repl-result))
          (saved-point (point-marker)))
      (goto-char marker)
      (slime-propertize-region `(face slime-repl-result-face
                                      rear-nonsticky (face))
        (slime-insert-image-presentation image id))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point))
      (set-marker slime-output-end (point))
      ;; Restore point before insertion but only it if was farther
      ;; than `marker'. Omitting this breaks REPL test
      ;; `repl-type-ahead'.
      (when (> saved-point (point))
        (goto-char saved-point)))
    (slime-repl-show-maximum-output)))

(defun slime-toggle-image-presentation-at-mouse (event)
  (interactive "@e")
  (multiple-value-bind (presentation start end buffer)
      (slime-presentation-around-click event)
    (slime-toggle-image-presentation presentation start end buffer)))

(defun slime-toggle-image-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation start end)
      (slime-presentation-around-or-before-point-or-error point)
    (slime-toggle-image-presentation presentation start end (current-buffer))))

(defun slime-toggle-image-presentation (presentation start end buffer)
  (with-current-buffer buffer
    (save-excursion
      (let ((image-p (get-char-property start 'image))
            (id (slime-presentation-id presentation)))
        (delete-region start end)
        (goto-char start)
        (if image-p
            (slime-insert-image-presentation-stub
             (slime-eval `(cl:fourth (swank-halos:make-halo-dispatch ,id)))
             id)
          (let ((image-data (slime-eval
                             `(cl:third (swank-halos:make-halo-dispatch ,id)))))
            (slime-insert-image-presentation
             (create-image (mapconcat 'byte-to-string
                                      (first image-data)
                                      "")
                           (intern (second image-data))
                           t)
             id)))))))

;; Patched this function, added to menu options
(defun slime-menu-choices-for-presentation (presentation buffer from to choice-to-lambda)
  "Return a menu for `presentation' at `from'--`to' in `buffer', suitable for `x-popup-menu'."
  (let* ((what (slime-presentation-id presentation))
         (choices (with-current-buffer buffer
                    (slime-eval
                     `(swank::menu-choices-for-presentation-id ',what)))))
    (cl-labels ((savel (f) ;; IMPORTANT - xemacs can't handle lambdas in x-popup-menu. So give them a name
                       (let ((sym (cl-gensym)))
                         (setf (gethash sym choice-to-lambda) f)
                         sym)))
               (etypecase choices
                 (list
                  `(,(format "Presentation %s" (truncate-string-to-width
                                                (slime-presentation-text presentation)
                                                30 nil nil t))
                    (""
                     ("Find Definition" . ,(savel 'slime-M-.-presentation-at-mouse))
                     ("Inspect" . ,(savel 'slime-inspect-presentation-at-mouse))
                     ("Describe" . ,(savel 'slime-describe-presentation-at-mouse))
                     ("Pretty-print" . ,(savel 'slime-pretty-print-presentation-at-mouse))
                     ("Copy to REPL" . ,(savel 'slime-copy-presentation-at-mouse-to-repl))
                     ("Copy to kill ring" . ,(savel 'slime-copy-presentation-at-mouse-to-kill-ring))
                     ,@(unless buffer-read-only
                         `(("Copy to point" . ,(savel 'slime-copy-presentation-at-mouse-to-point))))
                     ;; TODO slime-eval-async?
                     ,@(when (slime-eval `(cl:typep (swank:lookup-presented-object ,what)
                                                    'swank-halos:presentation-image))
                         `(("Toggle image" . ,(savel 'slime-toggle-image-presentation-at-mouse))))
                     ,@(let ((nchoice 0))
                         (mapcar
                          (lambda (choice)
                            (incf nchoice)
                            (cons choice
                                  (savel `(lambda ()
                                            (interactive)
                                            (slime-eval
                                             '(swank::execute-menu-choice-for-presentation-id
                                               ',what ,nchoice ,(nth (1- nchoice) choices)))))))
                          choices)))))
                 (symbol                ; not-present
                  (with-current-buffer buffer
                    (slime-remove-presentation-properties from to presentation))
                  (sit-for 0)           ; allow redisplay
                  `("Object no longer recorded"
                    ("sorry" . ,(if (featurep 'xemacs) nil '(nil)))))))))

(defun slime-dispatch-image-presentation-event (event)
  (slime-dcase event
    ((:presentation-insert-image id image-data string target)
     (slime-presentation-write-image-result
      (destructuring-bind (bytes image-format-string)
          image-data
        (create-image (mapconcat 'byte-to-string bytes "")
                      (intern image-format-string)
                      t))
      id)
     t)
    (t nil)))

;;;; ============================================================
;;;; These override slime's own functions. We need to restore the
;;;; original functions when our contrib is turned off.

(defun slime-defun-at-point ()
  "Return the text of the defun at point."
  (apply #'slime-buffer-substring-with-reified-output
         (slime-region-for-defun-at-point)))

(defun slime-last-expression ()
  (slime-buffer-substring-with-reified-output
   (save-excursion (backward-sexp) (point))
   (point)))

(defun slime-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (slime-eval-with-transcript
   `(swank:interactive-eval-region
     ,(slime-buffer-substring-with-reified-output start end))))

(defun slime-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((bounds (slime-bounds-of-sexp-at-point)))
    (if bounds
        (slime-buffer-substring-with-reified-output
         (car bounds)
         (cdr bounds)))))

;; ----------

(defun slime-eval-present-last-expression (string)
  "Similar to SLIME-EVAL-PRINT-LAST-EXPRESSION, evaluate sexp before point; present its values into the current buffer"
  (interactive (list (slime-last-expression)))
  (insert "\n")
  (slime-eval-async
      `(swank:eval-and-grab-presentations ,string)
    (lambda (result)
      (cl-destructuring-bind (output values) result
        (push-mark)
        (insert output)
        (loop for (string id) in values
              do
              (slime-insert-presentation string id t)
              (insert "\n"))))))

(provide 'slime-halos)
