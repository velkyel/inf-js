;;; inf-js.el --- Run an external Js process in an Emacs buffer -*- lexical-binding: t; -*-

;; * Add the following lines to your .emacs file:
;;
;;    (require 'inf-js)

;;    (setq inf-js-program "~/js/flisp")
;;    or
;;    (setq inf-js-program '("localhost" . 5555))

;;    (add-hook 'js2-mode-hook #'inf-js-minor-mode)

(require 'comint)
(require 'js2-mode)


(defgroup inf-js nil
  "Run an external js process (REPL) in an Emacs buffer."
  :group 'js2-mode)

(defcustom inf-js-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'inf-js)

;; TODO:
(defcustom inf-js-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior js's input history.
Input matching this regexp is not saved on the input history in Inferior js
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inf-js)

(defvar inf-js-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-x\C-e" #'inf-js-eval-last-sexp)
    (define-key map "\C-c\C-l" #'inf-js-load-file)
    (define-key map "\C-c\M-o" #'inf-js-clear-repl-buffer)
    map))

(defvar inf-js-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x"  #'inf-js-eval-defun)     ; Gnu convention
    (define-key map "\C-x\C-e" #'inf-js-eval-last-sexp) ; Gnu convention
    (define-key map "\C-c\C-e" #'inf-js-eval-last-sexp)
    (define-key map "\C-c\C-c" #'inf-js-eval-defun)     ; SLIME/CIDER style
    (define-key map "\C-c\C-b" #'inf-js-eval-buffer)
    (define-key map "\C-c\C-r" #'inf-js-eval-region)
    (define-key map "\C-c\C-n" #'inf-js-eval-form-and-next)
    (define-key map "\C-c\C-z" #'inf-js-switch-to-repl)
    (define-key map "\C-c\C-l" #'inf-js-load-file)
    map))

;;;###autoload
(define-minor-mode inf-js-minor-mode
  "Minor mode for interacting with the inferior js process buffer.

The following commands are available:

\\{inf-js-minor-mode-map}"
  :lighter "" :keymap inf-js-minor-mode-map
  nil)

(defcustom inf-js-program "flisp"
  "The command used to start an inferior js process in `inf-js-mode'.

Alternative you can specify a TCP connection cons pair, instead
of command, consisting of a host and port
number (e.g. (\"localhost\" . 5555)).  That's useful if you're
often connecting to a remote REPL process."
  :type '(choice (string)
                 (cons string integer))
  :group 'inf-js)

(defcustom inf-js-load-command "(load \"%s\")\n"
  "Format-string for building a js expression to load a file."
  :type 'string
  :group 'inf-js)

(defcustom inf-js-prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the Inferior js mode."
  :type 'regexp
  :group 'inf-js)

(defcustom inf-js-subprompt " *#_=> *"
  "Regexp to recognize subprompts in the Inferior js mode."
  :type 'regexp
  :group 'inf-js)

(defvar inf-js-buffer nil)

(defvar inf-js-mode-hook '()
  "Hook for customizing Inferior js mode.")

(put 'inf-js-mode 'mode-class 'special)

(define-derived-mode inf-js-mode comint-mode "Inferior js"
  (setq comint-prompt-regexp inf-js-prompt)
  (setq mode-line-process '(":%s"))
  ;; (scheme-mode-variables)
  (setq comint-get-old-input #'inf-js-get-old-input)
  (setq comint-input-filter #'inf-js-input-filter)
  (set (make-local-variable 'comint-prompt-read-only) inf-js-prompt-read-only)
  (add-hook 'comint-preoutput-filter-functions #'inf-js-preoutput-filter nil t))

(defun inf-js-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inf-js-input-filter (str)
  "Return t if STR does not match `inf-js-filter-regexp'."
  (not (string-match inf-js-filter-regexp str)))

(defun inf-js-chomp (string)
  "Remove final newline from STRING."
  (if (string-match "[\n]\\'" string)
      (replace-match "" t t string)
    string))

(defun inf-js-remove-subprompts (string)
  "Remove subprompts from STRING."
  (replace-regexp-in-string inf-js-subprompt "" string))

(defun inf-js-preoutput-filter (str)
  "Preprocess the output STR from interactive commands."
  (cond
   ((string-prefix-p "inf-js-" (symbol-name (or this-command last-command)))
    ;; Remove subprompts and prepend a newline to the output string
    (inf-js-chomp (concat "\n" (inf-js-remove-subprompts str))))
   (t str)))

(defvar inf-js-project-root-files    ;; TODO
  '("_darcs")
  "A list of files that can be considered project markers.")

(defun inf-js-project-root ()
  "Retrieve the root directory of a project if available.

Fallback to `default-directory.' if not within a project."
  (or (car (remove nil
                   (mapcar (lambda
                             (file)
                             (locate-dominating-file default-directory file))
                           inf-js-project-root-files)))
      default-directory))

(defun inf-js-clear-repl-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;;###autoload
(defun inf-js (cmd)
  (interactive (list (if current-prefix-arg
                         (read-string "Run js: " inf-js-program)
                       inf-js-program)))
  (if (not (comint-check-proc "*inf-js*"))
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory (inf-js-project-root))
            (cmdlist (if (consp cmd)
                         (list cmd)
                       (split-string cmd))))
        (set-buffer (apply #'make-comint
                           "inf-js" (car cmdlist) nil (cdr cmdlist)))
        (inf-js-mode)))
  (setq inf-js-buffer "*inf-js*")
  (pop-to-buffer-same-window "*inf-js*"))

;;;###autoload
(defalias 'run-js 'inf-js)

(defun inf-js-eval-region (start end &optional and-go)
  (interactive "r\nP")
  ;; replace multiple newlines at the end of the region by a single one
  ;; or add one if there was no newline
  (let ((str (concat (format "[%s:%d]" (buffer-name) (line-number-at-pos start t))
                     (replace-regexp-in-string
                      "[\n]*\\'" "\n"
                      (buffer-substring-no-properties start end)))))
    (comint-send-string (inf-js-proc) str))
  (if and-go (inf-js-switch-to-repl t)))

(defun inf-js-eval-string (code)
  (comint-send-string (inf-js-proc) (concat code "\n")))

(defun inf-js-eval-defun (&optional and-go)
  (interactive "P")
  (save-excursion
    (js2-end-of-defun)
    (let ((end (point)) (case-fold-search t))
      (js2-beginning-of-defun)
      (inf-js-eval-region (point) end and-go))))

(defun inf-js-eval-buffer (&optional and-go)
  (interactive "P")
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (inf-js-eval-region (point-min) (point-max) and-go))))

(defun inf-js-eval-last-sexp (&optional and-go)
  (interactive "P")
  (inf-js-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun inf-js-eval-form-and-next ()
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (inf-js-eval-last-sexp)
  (forward-sexp))

(defun inf-js-switch-to-repl (eob-p)
  "Switch to the inferior process buffer.
With prefix argument EOB-P, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-js-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inf-js-buffer t))))
        (pop-to-buffer inf-js-buffer))
    (run-js inf-js-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))


;;; Now that inf-js-eval-/defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun inf-js-eval-region-and-go (start end)
  (interactive "r")
  (inf-js-eval-region start end t))

(defun inf-js-eval-defun-and-go ()
  (interactive)
  (inf-js-eval-defun t))

(defvar inf-js-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `inf-js-load-file' command.")

(defcustom inf-js-source-modes '(js2-mode)
  "Used to determine if a buffer contains source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a js source file by `inf-js-load-file'.
Used by this command to determine defaults."
  :type '(repeat symbol)
  :group 'inf-js)

(defun inf-js-load-file (file-name)
  "Load a source file FILE-NAME into the inferior js process."
  (interactive (comint-get-source "Load file: " inf-js-prev-l/c-dir/file
                                  inf-js-source-modes nil)) ; nil because LOAD
                                        ; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq inf-js-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                            (file-name-nondirectory file-name)))
  (comint-send-string (inf-js-proc)
                      (format inf-js-load-command file-name))
  (inf-js-switch-to-repl t))

(defun inf-js-connected-p ()
  (not (null inf-js-buffer)))

;;; Documentation functions

;;; Command strings
;;; ===============


(defun inf-js-proc ()
  "Return the current inferior process.
See variable `inf-js-buffer'."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inf-js-mode)
                                      (current-buffer)
                                    inf-js-buffer))))
    (or proc
        (error "No js subprocess; see variable `inf-js-buffer'"))))

(provide 'inf-js)

