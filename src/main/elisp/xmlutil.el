;;; xmlutil.el --- Utility functions for XML processing

;;; Commentary:
;;

;;; Code:

(defcustom xmlutil-default-action-alist
  '(("http://www.w3.org/1999/XSL/Format" xmlutil--execute-fop)
    ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlutil--execute-rapper))
  "Alist of default actions for particular namespace URIs.

Each element is a cons with the namespace URI in car and a list
of functions to call in cdr."
  :group 'xmlutil
  :type '(list (cons string list)))

(defcustom xmlutil-namespace-context-alist '()
  "Alist of namespace URIs indexed by a common prefix.

Each element is a cons with the common prefix in car and the
namespace URI in cdr."
  :group 'xmlutil
  :type '(list (cons string string)))

(defun xmlutil-resolve-prefix (arg)
  "Insert namespace URI at point."
  (interactive "P")
  (let ((completion-ignore-case t))
    (let ((prefix (completing-read "Namespace prefix: " (sort (mapcar #'car xmlutil-namespace-context-alist) #'string-lessp))))
      (let ((uri (cdr (assoc prefix xmlutil-namespace-context-alist))))
        (when uri
          (if arg (kill-new uri)
            (insert uri)))))))

(defun xmlutil-default-action (arg)
  "Execute default action for current buffer."
  (interactive "P")
  (when (derived-mode-p 'nxml-mode)
    (dolist (action (cdr (assoc (xmlutil-outermost-namespace-uri) xmlutil-default-action-alist)))
      (funcall action arg))))

(defun xmlutil-format-region (beg end)
  "Indent XML content in current region."
  (interactive "r")
  (shell-command-on-region beg end "xmllint --format -" t t))

(defun xmlutil--execute-fop ()
  "Execute Apache FOP."
  (unless (buffer-file-name)
    (error "Buffer %s not visiting a file" (buffer-name)))
  (when (buffer-modified-p)
    (save-buffer))
  (let ((filename (expand-file-name (buffer-file-name))))
    (shell-command
     (format "%s -pdf %s.pdf %s"
             (executable-find "fop")
             (shell-quote-argument (file-name-sans-extension filename))
             (shell-quote-argument filename)))))

(defun xmlutil--execute-rapper (arg)
  "Execute rapper."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Buffer %s not visiting a file" (buffer-name)))
  (when (buffer-modified-p)
    (save-buffer))
  (let ((filename (expand-file-name (buffer-file-name)))
        (outfmt (if arg
                    (completing-read "Output format: " '("ntriples" "rdfxml" "rdfxml-abbrev" "turtle" "html" "json" "dot"))
                  "turtle")))
    (shell-command
     (format "%s -q -i rdfxml -o %s %s"
             (executable-find "rapper")
             outfmt
             (shell-quote-argument filename))
     (get-buffer-create "*Rapper Output*"))))

(defun xmlutil-outermost-namespace-uri ()
  "Return namespace URI of the outermost element."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (while (and (xmltok-forward)
                  (not (eq 'start-tag xmltok-type))))
      (when (eq 'start-tag xmltok-type)
        (xmlutil--nxml-namespace-uri-by-prefix
         (xmltok-start-tag-prefix) xmltok-namespace-attributes)))))

(defun xmlutil--nxml-namespace-uri-by-prefix (prefix bindings)
  "Return URI for PREFIX defined in BINDINGS."
  (let (uri)
    (while (and bindings (null uri))
      (when
          (or (and (null prefix)
                   (null (xmltok-attribute-prefix (car bindings)))
                   (string= "xmlns" (xmltok-attribute-local-name (car bindings))))
              (and prefix
                   (string= "xmlns" (xmltok-attribute-prefix (car bindings)))
                   (string= prefix  (xmltok-attribute-local-name (car bindings)))))
        (setq uri (xmltok-attribute-value (car bindings))))
      (setq bindings (cdr bindings)))
    uri))

(provide 'xmlutil)

;;; xmlutil.el ends here
