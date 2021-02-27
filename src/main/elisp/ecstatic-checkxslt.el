;;; ecstatic-checkxslt.el --- Flymake backend to check XSLT stylesheet for static errors -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defcustom ecstatic-checkxslt-command "checkxsltrepl"
  "Command required to start a CheckXstlRepl.
A string with an executable starting a CheckXstlRepl or a list
with the executable followed by arguments."
  :group 'ecstatic
  :type 'list)

(defvar-local ecstatic-checkxslt-process nil
  "Buffer-local CheckXsltRepl process.")

(defvar ecstatic-checkxslt-message-re
  "^\\(error\\|warning\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)$")

(define-minor-mode ecstatic-checkxslt
  "Use CheckXsltRepl in current buffer."
  :lighter " CheckXsltRepl"
  (if ecstatic-checkxslt
      (ecstatic-checkxslt-enable)
    (ecstatic-checkxslt-disable)))

(defun ecstatic-checkxslt-enable ()
  "Enable CheckXsltRepl in current buffer."
  (unless (memq 'ecstatic-checkxslt-flymake flymake-diagnostic-functions)
    (add-hook 'flymake-diagnostic-functions #'ecstatic-checkxslt-flymake 0 t)))

(defun ecstatic-checkxslt-disable ()
  "Disable CheckXsltRepl in current buffer."
  (remove-hook 'flymake-diagnostic-functions #'ecstatic-checkxslt-flymake t)
  (when (process-live-p ecstatic-checkxslt-process)
    (kill-process ecstatic-checkxslt-process)))

(defun ecstatic-checkxslt-flymake (report-fn &rest _args)
  "Flymake backend function.
REPORT-FN is the Flymake reporting function."
  (unless (process-live-p ecstatic-checkxslt-process)
    (setq ecstatic-checkxslt-process (ecstatic-checkxslt-make-process))
    (set-process-filter ecstatic-checkxslt-process (ecstatic-checkxslt-make-process-filter report-fn)))
  (save-excursion
    (save-restriction
      (widen)
      (process-send-region ecstatic-checkxslt-process (point-min) (point-max))
      (process-send-string ecstatic-checkxslt-process ""))))

(defun ecstatic-checkxslt-make-process ()
  "Create CheckXsltRepl process in current buffer."
  (let ((command (if (consp ecstatic-checkxslt-command)
                     ecstatic-checkxslt-command
                   (list ecstatic-checkxslt-command))))
    (unless (executable-find (car command))
      (error "Cannot locate CheckXsltRepl command executable"))
    (make-process :name "CheckXsltRepl"
                  :command command)))

(defun ecstatic-checkxslt-make-process-filter (report-fn)
  "Create a process filter.
REPORT-FN is the Flymake reporting function."
  (let ((source-buffer (current-buffer)))
    (lambda (_process content)
      (let (diags)
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (while (re-search-forward ecstatic-checkxslt-message-re nil t)
            (let ((message (match-string 4))
                  (region (flymake-diag-region source-buffer (string-to-number (match-string 2))))
                  (type (if (string= "warning" (match-string 1)) :warning :error)))
              (message "CheckXstlRepl: %s %s" type message)
              (push (flymake-make-diagnostic source-buffer (car region) (cdr region) type message) diags))))
        (funcall report-fn diags :force t)))))

(defun ecstatic-checkxslt-stylesheet-p ()
  "Return non-nil if current document is a XSLT stylesheet."
  (or (string= "http://www.w3.org/1999/XSL/Transform" (ecstatic-outermost-namespace-uri))
      (and buffer-file-name (member (file-name-extension buffer-file-name) '("xsl" "xslt")))))

(provide 'ecstatic-checkxslt)

;;; ecstatic-checkxslt.el ends here
