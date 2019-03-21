(defun map-bindings (prefix buffer)
  (let ((buffer (or buffer (current-buffer)))
        (re-exclude (regexp-opt
                     '("<vertical-line>" "<bottom-divider>" "<right-divider>"
                       "<mode-line>" "<C-down-mouse-2>" "<left-fringe>"
                       "<right-fringe>" "<header-line>"
                       "<vertical-scroll-bar>" "<horizontal-scroll-bar>")))
        res)
    (with-temp-buffer
      (let ((indent-tabs-mode t))
        (describe-buffer-bindings buffer prefix))
      (goto-char (point-min))
      ;; Skip the "Key translations" section
      (re-search-forward "")
      (forward-char 1)
      (while (not (eobp))
        (when (looking-at "^\\([^\t\n]+\\)[\t ]*\\(.*\\)$")
          (let ((key (match-string 1))
                (fun (match-string 2))
                cmd)
            (unless (or (member fun '("??" "self-insert-command"))
                        (string-match re-exclude key)
                        (not (commandp (setq cmd (intern-soft fun)))))
              (push
               (cons (cons key fun) cmd)
               res))))
        (forward-line 1)))
    (nreverse res)))



(defun build-entry (keyparts key descript cmd)
  (let* ((doc (documentation cmd))
        (doc-string (if (not doc) "" doc)))
  (goto-char (max-char))
  (insert
   (format
    "** %s
:PROPERTIES:
:CUSTOM_ID: %s
:PROPERTIES:
*** Function
%s
*** Documentation
%s\n"
    key (string-join keyparts) descript (replace-regexp-in-string (rx bol "*") "" doc-string)))
  ))


(defun build-org-file (prefix)
  (with-temp-file "~/doom-abcs/index.org"
    (org-mode)
    (build-table)
    (let ((bindings (map-bindings prefix (current-buffer))))
      (-reduce-from
       (lambda (acc it)
         (let* ((keymap (car it))
                (cmd (cdr it))
                (key (car keymap))
                (keyparts (split-string key " "))
                (pfx (-take 2  keyparts))
                (descript (cdr keymap)))
           (if (not (equal acc pfx))
               (progn
                 (add-table pfx)
                 (add-row key descript nil)
                 (build-entry keyparts key descript cmd)
                 pfx
                 )

             (progn
               (add-row key descript )
               (build-entry keyparts key descript cmd)
               acc
               ))))
       ""
       bindings))))
(build-org-file (vector (string-to-char (kbd doom-leader-key))))

(defun build-table()
    (insert "
* Overview
:PROPERTIES:
:CUSTOM_ID: OVERVIEW
:PROPERTIES:

* Documentation
:PROPERTIES:
:CUSTOM_ID: DOCUMENTATION
:PROPERTIES:
"))

(defun add-table(key-parts) 
    (goto-char (point-min))
    (re-search-forward (rx bol "* Documentation"))
    (previous-line)
    (insert (format 
"** %s \n#+NAME: %s\n" (string-join key-parts " ") (string-join key-parts)))

    (org-table-create "2x1")
    (org-table-goto-column 1)
    (insert "Key")
    (org-table-goto-column 2)
    (insert "Command")
    (org-table-hline-and-move))

(defun add-row(key descript insert)
  (goto-char (point-min))
  (re-search-forward "* Documentation")
  (re-search-backward "#\\+NAME:")
  (next-line)
  (goto-char(org-table-end))
  (forward-line -1)
  (if insert (org-table-insert-row))
  (org-table-goto-column 1)
  (insert key)
  (org-table-goto-column 2)
  (insert descript)
  (org-table-align)
  )

(build-table)(add-table '("SPC" "RET"))
(add-row '("SPC" "RET"))
