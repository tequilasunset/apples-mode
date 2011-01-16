;;; as-mode.el --- Major mode for editing and executing AppleScript code

;; Copyright (C) 2011 tequilasunset

;; Author: tequilasunset <tequilasunset.mac@gmail.com>
;; Keywords: AppleScript, languages
(defconst as-mode-version "0.0.1"
  "The Current version of `as-mode'.")

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program provides a major mode for AppleScript.

;; [INSTALL]
;;
;; Put files in your load-path and add the following to your init file.
;;
;;    (autoload 'as-mode "as-mode" "Happy AppleScripting!" t)
;;    (autoload 'as-open-scratch "as-mode" "Open scratch buffer for AppleScript." t)
;;    (add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . as-mode))
;; or
;;    (require 'as-mode)
;;    (add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . as-mode))
;;
;; After that you should byte-compile as-mode.el.
;;
;;    M-x byte-compile-file RET /path/to/as-mode.el RET
;;
;; During the byte-compilation, you may get some warnings, but you should
;; ignore them.
;;
;; [FEATURES]
;;
;; Commands for the execution have the prefix `as-run-'. You can see
;; the other features via menu.
;;
;; [CONFIGURATION]
;;
;; You can access to the customize group via menu or using the command
;; `as-customize-group'.
;;
;; Here is my configuration.
;;
;; (global-set-key (kbd "C-c a") 'as-open-scratch)
;; (setq as-underline-syntax-class "w")
;; (setq backward-delete-char-untabify-method 'all)
;; (setq imenu-auto-rescan t)
;; (defun my-as-mode-hook ()
;;   (local-set-key (kbd "<S-tab>") 'as-toggle-indent)
;;   (local-set-key (kbd "RET") 'newline-and-indent)
;;   (local-set-key (kbd "DEL") 'backward-delete-char-untabify)
;;   (setq imenu-case-fold-search nil)
;;   (imenu-add-menubar-index))
;; (add-hook 'as-mode-hook 'my-as-mode-hook)
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'as-mode)
;;      (add-hook 'as-mode-hook
;;                (lambda ()
;;                  (add-to-list 'ac-sources 'ac-source-applescript)))))

;;; Tested:

;; GNU Emacs 23.2.1 on Mac OS X 10.6.6

;;; Known Bugs:

;; In some cases, AppleScript's `display' command doesn't work correctly.

;;; TODO:

;; Syntax table
;; Open region or buffer's contents in AppleScript Editor and execute it.

;;; Code:

(require 'cl)
(require 'easymenu)
(require 'newcomment)

(defgroup as nil
  "Major mode for editing and executing AppleScript code."
  :group 'languages
  :prefix "as-")


;;; Utilities for internal use

(defconst as-identifier "\\(?:\\sw\\|\\s_\\)+") ; "[[:alnum:]_]+"

(defmacro as-define-show-func (name var)
  "Define `as-show-NAME', which is the command to display VAR."
  `(defun ,(intern (format "as-show-%s" name)) ()
     (interactive)
     (message "%s" (or ,var ""))))

(as-define-show-func mode-version as-mode-version)

(defsubst as-replace-re-comma->spaces (re)
  "Replace all `,'s with `\\\\s-+' in RE."
  (replace-regexp-in-string "," "\\\\s-+" re))

;; DB
(defvar as-plist nil)
(defsubst as-plist-put (prop val)
  (setq as-plist (plist-put as-plist prop val))
  val)
(defsubst as-plist-get (prop)
  (plist-get as-plist prop))

;; temp files
(defcustom as-tmp-dir nil
  "Specify the path of temp dir. If nil, temp dir will be created to
the same directory where as-mode.el is located."
  :type '(choice directory (const nil))
  :group 'as)

(defmacro as-define-tmp-file (name)
  "Define `as-tmp-NAME'."
  `(progn
     (defvar ,(intern (format "as-tmp-%s" name)) nil)
     (let ((files (as-plist-get :tmp-files))
           (file ',(intern (format "as-tmp-%s" name))))
       (unless (memq file files)
         (as-plist-put :tmp-files (cons file files)))
       file)))

(defun as-tmp-files-setup ()
  "Make the as-tmp-dir and temp files."
  (when (as-plist-get :tmp-files)
    (let ((dir (expand-file-name
                (or as-tmp-dir
                    (concat (file-name-directory (locate-library "as-mode"))
                            "as-tmp-dir")))))
      (unless (file-directory-p dir)
        (mkdir dir t))
      (loop for file in (as-plist-get :tmp-files)
            for tmp = (format "%s/%s.applescript" dir file)
            do
            (set file tmp)
            (unless (file-exists-p tmp)
              (with-temp-file tmp))
            finally (as-plist-put :tmp-files nil)))))


;;; User variables

(defcustom as-follow-error-position t
  "If non-nil, automatically move to the beginning position
where error has occurred."
  :type 'boolean
  :group 'as)

(defcustom as-prefer-coding-system nil
  "Specify the coding-system used for the execution of script."
  :type '(choice symbol (const nil))
  :group 'as)

(defcustom as-decompile-callback 'as-handle-decompile
  "Function to handle decompiled text via `as-decompile'.
It is required two arguments SCRIPT and FILENAME.
SCRIPT is decompiled script. FILENAME is decompiled file name."
  :type 'function
  :group 'as)

(defcustom as-decompile-query nil
  "If valid char is specified, handle the output of decompiling
with specified one. Acceptable chars are as following:\n
?o - overwrite file by decompiled script
?i - insert decompiled script at point
?c - copy decompiled script in clipboard.\n
If you set `as-decompile-callback' to other value, this is ignored.
Because this variable is used in `as-handle-decompile'."
  :type '(choice character (const nil))
  :group 'as)

(defcustom as-continuation-char ?\x00AC
  "Continuation character (not sign in mathematics)."
  :type 'character
  :group 'as)

(defcustom as-indent-offset 4
  "Amount of offset per level of indentation for AppleScript."
  :type 'integer
  :group 'as)

(defcustom as-continuation-offset as-indent-offset
  "Extra offset for the lines whose previous line is terminated with
the continuation character. See also `as-continuation-char'."
  :type 'integer
  :group 'as)

(defcustom as-indenters
  '("considering" "else" "if" "ignoring" "on" "repeat" "tell" "try")
  "Leading words of previous line which invoke the indentation of
current line.\n
See also `as-deindenters', `as-indent-regexps' and `as-noindent-regexps'."
  :type '(repeat string)
  :group 'as)

(defcustom as-deindenters
  '("else" "end" "on")
  "Leading words of current line which invoke the deindentation.\n
See also `as-indenters', `as-indent-regexps' and `as-noindent-regexps'."
  :type '(repeat string)
  :group 'as)

(defcustom as-indent-regexps
  (mapcar
   (lambda (re) (concat "^\\s-*" (as-replace-re-comma->spaces re)))
   '(;; script foo
     "script,\\<"
     "using,terms,from"
     "with,timeout"
     "with,transaction"
     ))
  "Regexps match to previous line, which invoke the indentation of
current line. This variable has priority over `as-indenters'.\n
See also `as-deindenters' and `as-noindent-regexps'."
  :type '(repeat regexp)
  :group 'as)

(defcustom as-noindent-regexps
  (mapcar
   (lambda (re) (concat "^\\s-*" (as-replace-re-comma->spaces re)))
   '(;; if foo then bar
     "if\\>.+\\<then,\\<"
     ;; tell application "foo" to bar
     "tell,application,\"[[:alnum:]_ ]+\",to,\\<"
     ))
  "Regexps match to previous line, which invoke the no-indentation of
current line. It means that the indentation of current line will
be same as previous line's one. This variable has priority over
`as-indent-regexps'.\n
See also `as-indenters' and `as-deindenters'."
  :type '(repeat regexp)
  :group 'as)

(defcustom as-keymap
  '(("<S-tab>"   . as-toggle-indent)
    ("C-c t r"   . as-run-region/buffer)
    ("C-c t k"   . as-compile)
    ("C-c t d"   . as-decompile)
    ("C-c t 3"   . as-show-last-result)
    ("C-c t l"   . as-insert-continuation-char)
    ("C-c t RET" . as-insert-continuation-char-and-newline)
    ("C-c t o"   . as-open-dict-index)
    ;; ("C-c t s"   . as-send-to-applescript-editor)
    )
  "Alist of keybindings for `as-mode'. Each element should be the form
\(KEY . COMMAND). KEY must be a string read by `kbd'.
If the value is nil, nothing will be set."
  :type '(repeat (cons string symbol))  ; Can't set to nil via customize group?
  :group 'as)

(defcustom as-underline-syntax-class nil
  "Specify syntax class of `_' (underline).
For example, \"w\" means a word, \"_\" means a symbol.
If nil, treated as a symbol."
  :type '(choice string (const nil))
  :link '(info-link "(elisp)Syntax Class Table")
  :group 'as)

(defcustom as-mode-hook nil
  "Hook executed just after called `as-mode'."
  :type 'hook
  :group 'as)

(defcustom ac-source-applescript
  '((candidates . (as-keywords))
    (symbol . "d")
    (cache))
  "Source for keywords of AppleScript. This is an additional source for
`auto-complete-mode'."
  :type 'sexp
  :link '(url-link "http://github.com/m2ym/auto-complete")
  :group 'as)

;; Faces
(macrolet ((face (name &rest attrs)
                 `(defface ,(intern (format "as-%s" name))
                    '((t (,@attrs)))
                    ,(subst-char-in-string ?- ? (format "Face for %s." name))
                    :group 'as)))
  (face statements :inherit font-lock-keyword-face)
  (face commands :inherit font-lock-keyword-face :italic t)
  (face operators :inherit font-lock-type-face)
  (face labels :inherit font-lock-type-face :italic t)
  (face records :inherit font-lock-builtin-face)
  (face reserved-words :inherit font-lock-keyword-face :italic t)
  (face error :inherit font-lock-warning-face)
  (face standard-folders :inherit font-lock-constant-face)
  (face continuation-char :inherit escape-glyph)
  (face error-highlight :background "DeepPink3")
  (face result-prompt :inherit minibuffer-prompt)
  (face error-prompt :inherit font-lock-warning-face)
  )


;;; Process

(as-define-show-func last-result (as-plist-get :last-result))
(as-define-show-func last-raw-result (as-plist-get :last-raw-result))

(defmacro as-set-run-info (&optional pred beg buf)
  "If passed PRED, record BEG (or 1) and BUF (or current buffer).
If PRED is omitted or returns nil, delete stored info."
  (declare (indent 0))
  `(as-plist-put :run-info (if ,pred
                               (cons (or ,beg 1) (or ,buf (current-buffer)))
                             (cons nil nil))))

(defun as-delete-error-overlay ()
  (let ((ov (as-plist-get :err-ov)))
    (when (overlayp ov)
      (delete-overlay ov))))

(defun as-error-overlay-setup ()
  (let ((ov (as-plist-get :err-ov)))
    (if ov
        (move-overlay ov 1 1)
      (setq ov (make-overlay 1 1))
      (overlay-put ov 'face 'as-error-highlight)
      (as-plist-put :err-ov ov)))
  (add-hook 'post-command-hook 'as-delete-error-overlay nil t))

;; FIXME: This workaround is not a perfect. Sometimes get the error like the
;; followings.
;;
;;    A identifier can't go after this “"”.
;;    A property can’t go after this “\"”.
;;
;; The cause of them may be the coding of temp file.
(as-define-tmp-file 1713)
(defun as-error-1713-workaround (f/s)
  "Avoid AppleScript's error -1713.\n
   execution error: No user interaction allowed. (-1713)"
  ;; Ref: <http://macscripter.net/viewtopic.php?id=26334>
  (unless (file-exists-p f/s)
    (with-temp-file as-tmp-1713
      (insert f/s))
    (setq f/s as-tmp-1713))
  (as-do-applescript
   (format
    "tell application \"AppleScript Runner\" to do script \"%s\"" f/s)))

(defun as-parse-error (result)
  (destructuring-bind
      (err-ov (actual-beg . err-buf)
              &aux err-beg err-end err-type err-msg err-num unknown)
      (values (as-plist-get :err-ov) (as-plist-get :run-info))
    (if (string-match
         "\\([0-9]+\\):\\([0-9]+\\): \\([^:]+:\\) \\(.+\\) (\\(-[0-9]+\\))$"
         result)
        (progn
          (setq err-beg (string-to-number (match-string 1 result))
                err-end (string-to-number (match-string 2 result))
                err-type (match-string 3 result)
                err-msg (match-string 4 result)
                err-num (string-to-number (match-string 5 result)))
          (when actual-beg
            (setq err-beg (+ err-beg actual-beg)
                  err-end (+ err-end actual-beg))))
      (setq unknown t))
    (values
     unknown
     ;; If get the unknown error, below five values return nil.
     err-beg err-end err-type err-msg err-num
     ;; If executed script is an unopened file or called from the minibuffer,
     ;; err-buf returns nil.
     err-buf err-ov)))

(defun as-result (result status f/s)
  "Handle the result of AppleScript's execution.
If execution has done successfully, display the result.
If error has occurred, display the error.
In that case, if executed script is same as current buffer or in it,
also highlight the error region and go to the beginning of it if
`as-follow-error-position' is non-nil."
  (block nil
    (as-plist-put :last-raw-result result)
    (setq result
          (if (= status 1)
              ;; error
              (multiple-value-bind (unknown beg end type msg num buf ov)
                  (as-parse-error result)
                ;; -1713
                (when (= num -1713)
                  (return-from nil (as-error-1713-workaround f/s)))
                (when (and beg buf)
                  ;; highlight and move
                  (when as-follow-error-position
                    (switch-to-buffer buf)
                    (goto-char beg)
                    (deactivate-mark))
                  (move-overlay ov beg end buf))
                ;; res
                (if unknown
                    result
                  (concat (propertize (concat type " ") 'face 'as-error-prompt)
                          msg)))
            ;; no error
            (concat (propertize "Result: " 'face 'as-result-prompt) result)))
    (as-set-run-info)
    (message (as-plist-put :last-result result))))

(defsubst as-proc-live-p (proc)
  "Return non-nil if PROC is still running."
  (and (processp proc)
       (not (eq (process-status proc) 'exit))
       (= (process-exit-status proc) 0)
       t))

(defsubst as-buffer-string (&optional buffer-or-name)
  "Return contents of a currnet buffer or BUFFER-OR-NAME.
Also delete the entire contents of the buffer."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (zerop (buffer-size))
        ""
      (prog1 (buffer-substring-no-properties
              (point-min) (1- (point-max)))
        (erase-buffer)))))

(defsubst as-proc-failed-p (proc)
  "Return non-nil, if PROC has been failed."
  (= (process-exit-status proc) 1))

(defsubst as-proc-failed (msg buf)
  "Display BUF's contents or MSG added 'missed message.
After that, delete BUF's contents."
  (let ((str (as-buffer-string buf)))
    (message (or (and (not (string= str "")) str)
                 (concat msg "missed")))))

(defsubst as-encode-string (str)
  "Encode STR to `as-prefer-coding-system' if it is specified."
  (if as-prefer-coding-system
      (encode-coding-string str as-prefer-coding-system)
    str))

(defun as-do-applescript (filename-or-script &optional callback)
  "Execute FILENAME-OR-SCRIPT as AppleScript. CALLBACK is required
three arguments RESULT, EXIT-PROC-STATUS and FILENAME-OR-SCRIPT.
If CALLBACK is omitted, call `as-result'."
  (lexical-let* ((f/s filename-or-script)
                 (callback callback)
                 (buf (get-buffer-create " *as-do-applescript*"))
                 (args (if (file-exists-p f/s)
                           `(,f/s)
                         `("-e" ,(as-encode-string f/s))))
                 (old-proc (get-buffer-process buf))
                 (enable (if (as-proc-live-p old-proc)
                             (when (y-or-n-p "\
as-do-applescript: Process is still running; kill it? ")
                               (progn
                                 (kill-process old-proc)
                                 t))
                           t)))
    (when enable
      (set-process-sentinel
       (apply 'start-process "as-do-applescript" buf "osascript" args)
       (lambda (proc _)
         (funcall (or callback 'as-result)
                  (as-buffer-string buf)
                  (process-exit-status proc)
                  f/s))))))

;; Compile
(defun as-compile (filename)
  "Compile FILENAME."
  (interactive
   (list (read-file-name "File: " buffer-file-name buffer-file-name)))
  (lexical-let* ((filename (expand-file-name filename))
                 (msg (message "Compiling...")))
    (set-process-sentinel
     (start-process "as-compile" nil "osacompile" filename)
     (lambda (proc _)
       (if (as-proc-failed-p proc)
           (message "%smissed" msg)
         (message "%sdone" msg))))))

;; Decompile
(defun as-handle-decompile (script filename)
  "Default function to handle decompiled script.
To specify the default query, set `as-decompile-query'."
  (case (or as-decompile-query
            (ignore-errors
              (flet ((prop (s) (propertize (concat "[" (upcase s) "]")
                                           'face '(:weight bold))))
                (read-char
                 (format "%s%sverwrite file %snsert script %sopy script"
                         (propertize "Select: " 'face 'as-result-prompt)
                         (prop "o") (prop "i") (prop "c"))))))
    (?o (with-temp-file filename
          (insert script)))
    (?i (insert script))
    (?c (with-temp-buffer
          (insert script)
          (kill-ring-save (point-min) (point-max))))))

(defun as-decompile (filename)
  "Decompile FILENAME. See also `as-decompile-query' and
`as-decompile-callback'."
  (interactive
   (list (read-file-name "File: " buffer-file-name buffer-file-name)))
  (lexical-let* ((filename (expand-file-name filename))
                 (buf (get-buffer-create " *as-decompile*"))
                 msg)
    (when (file-exists-p filename)
      (setq msg (message "Decompiling..."))
      (set-process-sentinel
       (start-process "as-decompile" buf "osadecompile" filename)
       (lambda (proc _)
         (if (as-proc-failed-p proc)
             (as-proc-failed msg buf)
           (funcall as-decompile-callback
                    (as-buffer-string buf)
                    filename)))))))

;; FIXME: I don't know the way of reverting...
;; ;; Send script to AppleScript Editor
;; (defun as-open (&rest args)
;;   "Run command `open' with ARGS (sync)."
;;   (apply 'call-process "open" nil nil nil args))

;; (as-define-tmp-file send)
;; (defun as-send-to-applescript-editor ()
;;   "Send region or current buffer to AppleScript Editor and run it."
;;   (interactive)
;;   (multiple-value-bind (beg end)
;;       (if (use-region-p)
;;           (values (region-beginning) (region-end))
;;         (values (point-min) (point-max)))
;;     (let ((script (buffer-substring-no-properties beg end)))
;;       (with-temp-file as-tmp-send
;;         (insert script)))
;;     (when (= (as-open "-a" "AppleScript Editor" as-tmp-send) 0)
;;       (do-applescript
;;        (mapconcat
;;         'identity
;;         '("tell application \"System Events\""
;;           "    tell process \"AppleScript Editor\""
;;           "        key code 15 using command down" ; Command-R
;;           "    end tell"
;;           "end tell")
;;         "\n")))))


;;; Commands

;; Run
(defun as-run-file (&optional filename)
  "Execute FILENAME as AppleScript."
  (interactive "fFile: ")
  (setq filename (expand-file-name filename))
  (when (file-exists-p filename)
    (as-set-run-info
      (eq (get-file-buffer filename)
          (current-buffer)))
    (as-do-applescript filename)))

(defun as-run-buffer (&optional buffer-or-name)
  "Execute a current buffer or BUFFER-OR-NAME as AppleScript."
  (interactive)
  (or buffer-or-name (setq buffer-or-name (current-buffer)))
  (as-set-run-info
    (eq (or (get-buffer buffer-or-name)
            buffer-or-name)
        (current-buffer)))
  (with-current-buffer buffer-or-name
    (as-do-applescript (buffer-string))))

(defun as-run-region (beg end)
  "Execute region as AppleScript."
  (interactive "r")
  (when (/= beg end)
    (as-set-run-info t beg)
    (as-do-applescript (buffer-substring beg end))))

(defun as-run-region/buffer ()
  "Execute region or current buffer as AppleScript."
  (interactive)
  (if (use-region-p)
      (call-interactively 'as-run-region)
    (as-run-buffer)))

(defun as-run-minibuf (script)
  "Read script from minibuffer and execute it as AppleScript."
  (interactive "sScript: ")
  (as-do-applescript script))

;; Open Dictionary index
(defun as-open-dict-index ()
  "Open dictionary index in AppleScript Editor."
  (interactive)
  (do-applescript
   (mapconcat
    'identity
    '("tell application \"AppleScript Editor\" to activate"
      "tell application \"System Events\""
      "    tell process \"AppleScript Editor\""
      "        key code 31 using {shift down, command down}" ; Command-Shift-O
      "    end tell"
      "end tell")
    "\n")))

;; Insert continuation character
(defsubst as-continuation-char ()
  "Return the continuation character as a string."
  (char-to-string as-continuation-char))

(defun as-insert-continuation-char ()
  "Insert the continuation character."
  (interactive "^")
  (when (re-search-backward "\\s-+\\=" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (insert ? as-continuation-char))

(defun as-insert-continuation-char-and-newline ()
  "Insert the continuation character, then add newline."
  (interactive "^")
  (as-insert-continuation-char)
  (call-interactively (key-binding (kbd "RET"))))

;; Scratch buffer for AppleScript
(as-define-tmp-file scratch)
(defun as-save-scratch ()
  "Write the contents of *as-scratch* to temp file (as-tmp-scratch)."
  (let ((buf (get-buffer "*as-scratch*")))
    (when buf
      (with-current-buffer buf
        (write-region (point-min) (point-max) as-tmp-scratch nil 'quiet)))))

;;;###autoload
(defun as-open-scratch ()
  "Open scratch buffer for AppleScript."
  (interactive)
  (as-tmp-files-setup)
  (pop-to-buffer (get-buffer-create "*as-scratch*"))
  (add-hook 'kill-emacs-hook 'as-save-scratch)
  (add-hook 'kill-buffer-hook 'as-save-scratch nil t)
  (insert-file-contents as-tmp-scratch)
  (goto-char (point-max))
  (as-mode))

;; Key code
(defconst as-key-codes
  '((?a . 0) (?b . 11) (?c . 8) (?d . 2) (?e . 14) (?f . 3) (?g . 5) (?h . 4)
    (?i . 34) (?j . 38) (?k . 40) (?l . 37) (?m . 46) (?n . 45) (?o . 31)
    (?p . 35) (?q . 12) (?r . 15) (?s . 1) (?t . 17) (?u . 32) (?v . 9)
    (?w . 13) (?x . 7) (?y . 16) (?z . 6) (?0 . 29) (?1 . 18) (?2 . 19)
    (?3 . 20) (?4 . 21) (?5 . 23) (?6 . 22) (?7 . 26) (?8 . 28) (?9 . 25)
    (f1 . 122) (f2 . 120) (f3 . 99) (f4 . 118) (f5 . 96) (f6 . 97) (f7 . 98)
    (f8 . 100) (f9 . 101) (f10 . 109) (f11 . 103) (f12 . 111) (escape . 53)
    (tab . 48) (?  . 49) (return  . 36) (backspace . 51) (left . 123)
    (right . 124) (down . 125) (up . 126))
  "Index of key codes. Each element has the form (CHAR-OR-SYMBOL . KEY-CODE).")

(defun as-lookup-key->key-code ()
  "Look up key code of AppleScript from key."
  (interactive)
  (clear-this-command-keys)
  (message (propertize "Key: " 'face 'minibuffer-prompt))
  (let* ((key (read-event))
         (key-code (cdr (assq key as-key-codes)))
         (key (if (integerp key) (char-to-string key) key)))
    (if key-code
        (message (format "Key: %s  Key code: %d" key key-code))
      (message "Not found %s" key))))

(defun as-lookup-key-code->key (key-code)
  "Look up key from key code of AppleScript."
  (interactive "nKey code: ")
  (let* ((key (car (rassq key-code as-key-codes)))
         (key (if (integerp key) (char-to-string key) key)))
    (if key
        (message (format "Key: %s  Key code: %d" key key-code))
      (message "Not found %d" key-code))))

;; Comment
(defun as-comment-or-uncomment-region (beg end &optional arg)
  "`comment-or-uncomment-region' for `as-mode'."
  (interactive "*r\nP")
  (let ((comment-style 'indent))
    (comment-or-uncomment-region beg end arg)))

(defun as-comment-dwim (arg)
  "`comment-dwim' for `as-mode'."
  (interactive "*P")
  (let ((comment-style 'indent))
    (comment-dwim arg)))


;;; Indentation

(defsubst as-in-string/comment-p (&optional pos)
  "Return non-nil, if POS is in string or in comment."
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (or (nth 3 ppss) (nth 4 ppss)))))

(defsubst as-in-string-p (&optional pos)
  "Return non-nil, if POS is in string."
  (save-excursion
    (nth 3 (syntax-ppss pos))))

(defsubst as-ideal-prev-bol ()
  "Return the point of previous bol or nil. Empty lines, lines filled by
whitespaces and lines whose bol is in string or in comment are skipped."
  (save-excursion
    (loop initially (beginning-of-line)
          while (not (bobp))
          do (forward-line -1)
          unless (or (looking-at "\\s-*$")
                     (as-in-string/comment-p))
          return (point))))

(defsubst as-leading-word-of-line ()
  "Return the leading word of line as a string.
If leading char except whitespaces is not a word, return nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at (concat "\\s-*\\(" as-identifier "\\)"))
      (buffer-substring-no-properties
       (match-beginning 1) (match-end 1)))))

(defsubst as-line-string ()
  "Return the contents of current line as a string. Leading and trailing
whitespaces are deleted."
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq beg (point))
      (end-of-line)
      (skip-chars-backward " \t")
      (setq end (point)))
    (buffer-substring-no-properties beg end)))

(defsubst as-string-match (regexps string)
  "Unlike `string-match', first argument has to be a list of REGEXPS."
  (some (lambda (re) (string-match re string)) regexps))

(defun as-parse-lines ()
  "Parse current and previous lines then return the values."
  (let ((prev-bol (as-ideal-prev-bol))
        (cchar-re (concat (as-continuation-char) "\\s-*$"))
        prev-indent prev-lword prev-lstr pprev-bol prev-cchar-p pprev-cchar-p)
    (flet ((cchar? (lstr) (string-match cchar-re lstr)))
      (when prev-bol
        (save-excursion
          (goto-char prev-bol)
          (setq prev-indent (current-indentation)
                prev-lword (as-leading-word-of-line)
                prev-lstr (as-line-string)
                prev-cchar-p (cchar? prev-lstr))
          ;; Parse previous line again for continuation char.
          (when (setq pprev-bol (as-ideal-prev-bol))
            (goto-char pprev-bol)
            (setq pprev-cchar-p (cchar? (as-line-string))))))
      (values
       (current-column) (current-indentation) (as-leading-word-of-line)
       ;; If prev-bol is nil, belows return nil.
       prev-bol prev-indent prev-lword prev-lstr prev-cchar-p pprev-cchar-p))))

(defun as-indent-line ()
  "Indent current line according to AppleScript indentation rules."
  (interactive "^")
  (let* ((bol-ppss (save-excursion (syntax-ppss (point-at-bol))))
         (bol-is-in-string (nth 3 bol-ppss))
         (bol-is-in-comment (nth 4 bol-ppss))
         (pos (point))
         indent)
    (unless bol-is-in-string
      (multiple-value-bind
          (cur-col cur-indent cur-lword prev-bol prev-indent
                   prev-lword prev-lstr prev-cchar-p pprev-cchar-p)
          (as-parse-lines)
        (if bol-is-in-comment
            (setq indent (or prev-indent 0))
          ;; bol is neither in string nor in comment
          (flet ((match? (regs str) (and regs str (as-string-match regs str)))
                 (member? (str lst) (and str lst (member str lst))))
            (let* ((cchar-indent?   (and prev-cchar-p (not pprev-cchar-p)))
                   (prev-indent?    (match? as-indent-regexps prev-lstr))
                   (prev-noindent?  (match? as-noindent-regexps prev-lstr))
                   (cchar-deindent? (and (not prev-cchar-p) pprev-cchar-p))
                   (prev-indenter?  (member? prev-lword as-indenters))
                   (cur-deindenter? (member? cur-lword as-deindenters)))
              ;; Calculating...
              (setq indent (- (cond ((and cchar-indent? (not prev-noindent?))
                                     ;; indent by cchar offset
                                     (+ prev-indent as-continuation-offset))
                                    ((and (or prev-indent? prev-indenter?)
                                          (not prev-noindent?))
                                     ;; indent
                                     (+ prev-indent as-indent-offset))
                                    (cchar-deindent?
                                     ;; deindent by cchar offset
                                     (- prev-indent as-continuation-offset))
                                    ;; same as prev
                                    (prev-indent)
                                    ;; noindent
                                    (t cur-indent))
                              (if cur-deindenter?
                                  ;; deindent
                                  as-indent-offset
                                ;; noindent
                                0))))))
        ;; Now indent line.
        (indent-line-to (if (natnump indent) indent 0))
        (when (> (- cur-col cur-indent) 0)
          (goto-char (+ pos (- (current-indentation) cur-indent))))))))

(defun as-toggle-indent ()
  "Toggle indentation."
  (interactive "^")
  (unless (as-in-string-p (point-at-bol))
    (multiple-value-bind
        (cur-col cur-indent _1 _2 prev-indent _3 _4 prev-cchar-p pprev-cchar-p)
        (as-parse-lines)
      (let* ((pos (point))
             (diff (- prev-indent cur-indent))
             (offset (if (or prev-cchar-p pprev-cchar-p)
                         as-continuation-offset
                       as-indent-offset))
             (indent (cond ((> diff 0) prev-indent)
                           ((= diff 0) (+ prev-indent offset))
                           (t (- prev-indent offset)))))
        ;; Now indent line.
        (indent-line-to (if (natnump indent) indent 0))
        (when (> (- cur-col cur-indent) 0)
          (goto-char (+ pos (- (current-indentation) cur-indent))))))))


;;; Font lock

(defconst as-keywords
  `((reserved-words
     . ("about" "above" "after" "against" "and" "apart from" "around" "as"
        "aside from" "at" "back" "before" "beginning" "behind" "below" "beneath"
        "beside" "between" "but" "by" "considering" "contain" "contains"
        "contains" "continue" "copy" "div" "does" "eighth" "else" "end" "equal"
        "equals" "error" "every" "exit" "false" "fifth" "first" "for" "fourth"
        "from" "front" "get" "given" "global" "if" "ignoring" "in" "instead of"
        "into" "is" "it" "its" "last" "local" "me" "middle" "mod" "my" "ninth"
        "not" "of" "on" "onto" "or" "out of" "over" "prop" "property" "put"
        "ref" "reference" "repeat" "return" "returning" "script" "second" "set"
        "seventh" "since" "sixth" "some" "tell" "tenth" "that" "the" "then"
        "third" "through" "thru" "timeout" "times" "to" "transaction" "true"
        "try" "until" "where" "while" "whose" "with" "without"))
    (statements
     . ("application" "considering" "considering application responses"
        "continue" "else" "end" "error" "exit" "if" "ignoring"
        "ignoring application responses" "on" "repeat" "repeat until"
        "repeat while" "repeat with" "return" "tell" "then" "using terms from"
        "with timeout" "with transaction"))
    (commands
     . ("ASCII character" "ASCII number" "activate" "AGStart" "beep" "copy"
        "count" "choose application" "choose color" "choose file"
        "choose file name" "choose folder" "choose from list"
        "choose remote application" "choose URL" "clipboard info" "close access"
        "current date" "delay" "display alert" "display dialog"
        "do shell script" "get" "get eof" "get volume settings" "info for"
        "launch" "list disks" "list folder" "load script" "localized string"
        "log" "monitor depth" "max monitor depth" "min monitor depth"
        "mount volume" "new file" "offset" "open for access" "open location"
        "path to" "path to application" "path to folder" "path to resource"
        "random number" "read" "round" "run" "run script" "say"
        "scripting component" "set" "set eof" "set monitor depth"
        "set the clipboard to" "set volume" "start log" "stop log"
        "store script" "system attribute" "system info" "time to GMT"
        "the clipboard" "write"))
    (operators
     . ("&" "*" "+" "-" "/" "<" "<=" "=" ">" ">=" "^" "a ref to" "a ref"
        "a reference to" "a reference" "and" "as" "begin with" "begins with"
        "comes after" "comes before" "contain" "contains" "div"
        "does not come after" "does not come before" "does not contain"
        "does not equal" "doesn't come after" "doesn't come before"
        "doesn't contain" "doesn't equal" "end with" "ends with" "equal to"
        "equals" "greater than or equal to" "greater than or equal"
        "greater than" "in not contained by" "is contained by" "is equal to"
        "is equal" "is greater than or equal to" "is greater than or equal"
        "is greater than" "is in" "is less than or equal to"
        "is less than or equal" "is less than" "is not equal to" "is not equal"
        "is not greater than or equal to" "is not greater than or equal"
        "is not greater than" "is not in" "is not less than or equal to"
        "is not less than or equal" "is not less than" "is not"
        "isn't contained by" "isn't equal to" "isn't equal"
        "isn't greater than or equal to" "isn't greater than or equal"
        "isn't greater than" "isn't less than or equal to"
        "isn't less than or equal" "isn't less than" "isn't"
        "less than or equal to" "less than or equal" "less than" "mod" "not"
        "or" "ref to" "ref" "reference to" "reference" "start with"
        "starts with"))
    (handler-parameter-labels
     . ("about" "above" "against" "apart from" "around" "aside from" "at"
        "below" "beneath" "beside" "between" "by" "for" "from" "given"
        "instead of" "into" "on" "onto" "out of" "over" "since" "thru" "through"
        "under"))

    (standard-folders
     . ,(let ((lst
               ;; Ref: <http://macwiki.sourceforge.jp/wiki/index.php/AppleScript>
               ;;      <http://docs.info.apple.com/jarticle.html?path=AppleScript/2.1/en/as189>
               '(("application support"
                  "Macintosh HD:Library:Application Support:"
                  "/Library/Application Support/")
                 ("applications folder"
                  "Macintosh HD:Applications:"
                  "/Applications/")
                 ("current application"
                  "Macintosh HD:System:Library:CoreServices:AppleScript Runner.app:"
                  "/System/Library/CoreServices/AppleScript Runner.app/")
                 ("current user folder"
                  "Macintosh HD:Users:username:"
                  "/Users/username/")
                 ("desktop"
                  "Macintosh HD:Users:username:Desktop:"
                  "/Users/username/Desktop/")
                 ("desktop pictures folder"
                  "Macintosh HD:Library:Desktop Pictures:"
                  "/Library/Desktop Pictures/")
                 ("documents folder"
                  "Macintosh HD:Users:username:Documents:"
                  "/Users/username/Documents/")
                 ("downloads folder"
                  "Macintosh HD:Users:username:Downloads:"
                  "/Users/username/Downloads/")
                 ("favorites folder"
                  "Macintosh HD:Users:username:Library:Favorites:"
                  "/Users/username/Library/Favorites/")
                 ("Folder Action scripts"
                  "Macintosh HD:Users:username:Library:Scripts:Folder Action Scripts:"
                  "/Users/username/Library/Scripts/Folder Action Scripts/")
                 ("fonts"
                  "Macintosh HD:System:Library:Fonts:"
                  "/System/Library/Fonts/")
                 ("frontmost application"
                  "Macintosh HD:System:Library:CoreServices:Finder.app:"
                  "/System/Library/CoreServices/Finder.app/")
                 ("help folder"
                  "Macintosh HD:Library:Documentation:Help:"
                  "/Library/Documentation/Help/")
                 ("home folder"
                  "Macintosh HD:Users:username:"
                  "/Users/username/")
                 ("keychain folder"
                  "Macintosh HD:Users:username:Library:Keychains:"
                  "/Users/username/Library/Keychains/")
                 ("library folder"
                  "Macintosh HD:Library:"
                  "/Library/")
                 ("modem scripts"
                  "Macintosh HD:System:Library:Modem Scripts:"
                  "/System/Library/Modem Scripts/")
                 ("movies folder"
                  "Macintosh HD:Users:username:Movies:"
                  "/Users/username/Movies/")
                 ("music folder"
                  "Macintosh HD:Users:username:Music:"
                  "/Users/username/Music/")
                 ("pictures folder"
                  "Macintosh HD:Users:username:Pictures:"
                  "/Users/username/Pictures/")
                 ("preferences"
                  "Macintosh HD:Users:username:Library:Preferences:"
                  "/Users//username/Library/Preferences/")
                 ("printer descriptions"
                  "Macintosh HD:System:Library:Printers:PPDs:"
                  "/System/Library/Printers/PPDs/")
                 ("public folder"
                  "Macintosh HD:Users:username:Public:"
                  "/Users/username/Public/")
                 ("scripting additions"
                  "Macintosh HD:System:Library:ScriptingAdditions:"
                  "/System/Library/ScriptingAdditions/")
                 ("scripts folder"
                  "Macintosh HD:Users:username:Library:Scripts:"
                  "/Users/username/Library/Scripts/")
                 ("shared documents folder"
                  "Macintosh HD:Users:Shared:"
                  "/Users/Shared/")
                 ("shared libraries"
                  "Macintosh HD:System:Library:CFMSupport:"
                  "/System/Library/CFMSupport/")
                 ("sites folder"
                  "Macintosh HD:Users:username:Sites:"
                  "/Users/username/Sites/")
                 ("startup disk"
                  "Macintosh HD:"
                  "/")
                 ("system folder"
                  "Macintosh HD:System:"
                  "/System/")
                 ("system preferences"
                  "Macintosh HD:System:Library:PreferencePanes:"
                  "/System/Library/PreferencePanes/")
                 ("temporary items"
                  "Macintosh HD:private:var:folders:foobar:TemporaryItems:"
                  "/private/var/folders/foobar/TemporaryItems/")
                 ("trash"
                  "Macintosh HD:Users:username:.Trash:"
                  "/Users/username/.Trash/")
                 ("users folder"
                  "Macintosh HD:Users:"
                  "/Users/")
                 ("utilities folder"
                  "Macintosh HD:Applications:Utilities:"
                  "/Applications/Utilities/")
                 ("voices"
                  "Macintosh HD:System:Library:Speech:Voices:"
                  "/System/Library/Speech/Voices/")
                 )))
          (loop for (folder path posix) in (nreverse lst)
                collect (propertize folder 'path path 'posix posix))))
    )
  "Keywords of AppleScript. Each element has the form (TYPE . KEYWORDS).")


(defun as-keywords (&optional type)
  "Return keywords of TYPE. If it is omitted, return all keywords."
  (if type
      (cdr (assq type as-keywords))
    (apply 'append (mapcar 'cdr as-keywords))))

(defvar as-font-lock-keywords
  (let ((i as-identifier))
    (flet ((kws (type-or-types)
                (replace-regexp-in-string
                 " " "\\\\s-+"
                 (regexp-opt (as-keywords type-or-types) 'words)))
           (cat (&rest s) (as-replace-re-comma->spaces (apply 'concat s))))
      `(
        ("\\<error\\>"                     0 'as-error                    )
        (,(cat "\\<on,\\(" i "\\)")        1 font-lock-function-name-face )
        (,(cat "^to,\\(" i "\\)")          1 font-lock-function-name-face )
        (,(cat "\\<set,\\(" i "\\),to\\>") 1 font-lock-variable-name-face )
        (,(as-continuation-char)           0 'as-continuation-char        )
        (,(kws 'standard-folders)          1 'as-standard-folders         )
        (,(kws 'statements)                1 'as-statements               )
        (,(kws 'commands)                  1 'as-commands                 )
        (,(kws 'operators)                 1 'as-operators                )
        (,(cat "\\(,[-&*+/<=>^],\\|,<=,\\|,>=,\\)")
         0 'as-operators                )
        (,(cat "\\<" i ":")                0 'as-records                  )
        (,(kws 'handler-parameter-labels)  1 'as-labels                   )
        (,(kws 'reserved-words)            1 'as-reserved-words           )
        )))
  "Font lock keywords for `as-mode'.
See also `font-lock-defaults' and `font-lock-keywords'.")


;;; Misc

(defvar as-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Define menu.
    (easy-menu-define nil map
      "Menu for `as-mode'."
      `("AppleScript"
        ["Open Scratch" as-open-scratch]
        ["Open Dictionary..." as-open-dict-index]
        ("Edit" :visible (not buffer-read-only)
         ["(Un)comment Region" as-comment-or-uncomment-region]
         ["Comment Dwim" as-comment-dwim]
         ["Insert Continuation Char" as-insert-continuation-char]
         ["Insert Continuation Char and Newline"
          as-insert-continuation-char-and-newline]
         )
        ("Execution"
         ["Run File" as-run-file]
         ["Run Buffer" as-run-buffer]
         ["Run Region" as-run-region]
         ["Run from Minibuffer" as-run-minibuf]
         ["Run Region or Buffer" as-run-region/buffer]
         ;; ["Send Region or Buffer to ASE" as-send-to-applescript-editor]
         "---"
         ["Compile" as-compile]
         ["Decompile" as-decompile]
         "---"
         ["Last Result" as-show-last-result]
         ["Last Raw Result" as-show-last-raw-result]
         )
        ("Misc"
         ("Key <=> Key Code"
          ["Key => Key Code" as-lookup-key->key-code]
          ["Key Code => Key" as-lookup-key-code->key])
         ("path to..."
          ,@(loop for folder in (nreverse (as-keywords 'standard-folders))
                  collect (multiple-value-bind (path posix)
                              (with-temp-buffer
                                (insert folder)
                                (let ((pos (point-min)))
                                  (values (get-text-property pos 'path)
                                          (get-text-property pos 'posix))))
                            `(,(capitalize folder)
                              [,folder
                               (insert ,folder) (not buffer-read-only)]
                              [,(concat "PATH: " path)
                               (prin1 ,path (current-buffer))
                               (not buffer-read-only)]
                              [,(concat "POSIX: " posix)
                               (prin1 ,posix (current-buffer))
                               (not buffer-read-only)]))))
         )
        "---"
        ["Customizations" as-customize-group]
        ["Mode Version" as-show-mode-version]
        ["AppleScript Version" as-show-applescript-version]
        ["Visit as-mode Project" as-visit-project]
        ))
    map)
  "Keymap used in `as-mode'.")

(defun as-keymap-setup ()
  "Set up keybindings for `as-mode' according to `as-keymap'."
  (when (and as-keymap
             (not (as-plist-get :keybinded?)))
    (loop for (key . cmd) in as-keymap
          do (define-key as-mode-map (read-kbd-macro key) cmd)
          finally (as-plist-put :keybinded? t))))

;; TODO: Syntax table is still rough.
(defvar as-mode-syntax-table
  (let ((st (make-syntax-table))
        (lst
         '((?\" "\"")
           (?\\ "\\")
           (?:  "_")
           (?_  "_")
           (?-  ". 12")
           (?\t "    ")
           (?\f "    ")
           (?\n ">    ")
           (?\{ "(}")
           (?\} "){")
           (?\( "() 1b")
           (?\) ")( 4b")
           (?*  ". 23b")
           )))
    (loop for (char entry) in lst
          do (modify-syntax-entry char entry st))
    st)
  "Syntax table used in `as-mode'.")

(defun as-applescript-version ()
  "Return AppleScript's version."
  (unless (as-plist-get :AS-version)
    (let ((ver (with-temp-buffer
                 (when (= (call-process "osascript" nil (current-buffer) t
                                        "-e" "AppleScript's version")
                          0)
                   (as-buffer-string)))))
      (as-plist-put :AS-version ver)
      ver)))
(as-define-show-func applescript-version (as-plist-get :AS-version))

(defun as-customize-group ()
  (interactive)
  (customize-group "as"))

(defun as-visit-project ()
  (interactive)
  (browse-url "http://github.com/tequilasunset/as-mode"))

(defvar as-imenu-generic-expression
  (nreverse
   (let ((i as-identifier))
     (flet ((cat (&rest s) (as-replace-re-comma->spaces (apply 'concat s)))
            (ptn (title &rest re) (list title (apply 'cat "^\\s-*" re) 1)))
       (list
        (ptn "Handlers"     "\\(?:on\\|to\\),\\(" i "\\)"      )
        (ptn "Applications" "tell,application,\"\\(" i "\\)\"" )
        (ptn "Variables"    "set,\\(.+\\),to"                  )
        ))))
  "Imenu index pattern for AppleScript. See also `imenu-generic-expression'.")


;;; Major mode

;;;###autoload
(defun as-mode ()
  "Happy AppleScripting!"
  (interactive)
  (kill-all-local-variables)
  (as-tmp-files-setup)
  (as-error-overlay-setup)
  ;; map and table
  (as-keymap-setup)
  (use-local-map as-mode-map)
  (set-syntax-table as-mode-syntax-table)
  (when as-underline-syntax-class
    (modify-syntax-entry ?_ as-underline-syntax-class as-mode-syntax-table))
  ;; local variables
  (setq major-mode 'as-mode
        mode-name "AppleScript"
        font-lock-defaults '(as-font-lock-keywords)
        paragraph-separate "[ \t\n\f]*$"
        paragraph-start    "[ \t\n\f]*$"
        comment-start "-- "
        comment-end   ""
        comment-start-skip "\\(?:---\\|(\\*\\)+[ \t]*"
        comment-column 40
        indent-line-function 'as-indent-line
        imenu-generic-expression as-imenu-generic-expression
        )
  ;; AppleScript above v2.0 supports # comment.
  (let ((ver (as-applescript-version)))
    (when (and ver (>= (string-to-number ver) 2.0))
      (modify-syntax-entry ?# "<" as-mode-syntax-table)
      (setq comment-start-skip "\\(?:#\\|---\\|(\\*\\)+[ \t]*")))
  (run-hooks 'as-mode-hook))

(provide 'as-mode)
;;; as-mode.el ends here
