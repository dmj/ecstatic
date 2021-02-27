;;; ecstatic.el --- Utility functions for XML processing

;;; Commentary:
;;


;;; Code:

(defun ecstatic-outermost-namespace-uri ()
  "Return namespace URI of outermost element."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (while (and (xmltok-forward)
                  (not (eq 'start-tag xmltok-type))))
      (when (eq 'start-tag xmltok-type)
        (ecstatic-namespace-uri-by-prefix
         (xmltok-start-tag-prefix) xmltok-namespace-attributes)))))

(defun ecstatic-namespace-uri-by-prefix (prefix bindings)
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

(provide 'ecstatic)

;;; ecstatic.el ends here
