(defun doom-abcs/map-bindings (prefix buffer)
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

(defun doom-abcs/init-page()
    (insert "#+TITLE: Doom Emacs ABCs
#+SETUPFILE: https://fniessen.github.io/org-html-themes/setup/theme-readtheorg.setup
#+OPTIONS: num:nil

* Overview
:PROPERTIES:
:CUSTOM_ID: OVERVIEW
:END:

* Documentation
:PROPERTIES:
:CUSTOM_ID: DOCUMENTATION
:END:
"))

(defun doom-abcs/build-entry (key descript cmd)
  (let* ((doc (documentation cmd))
         (doc-string (if (not doc) "" doc))
         (uuid (org-id-uuid)))
    (goto-char (max-char))
    (insert
     (format
      "** %s
:PROPERTIES:
:CUSTOM_ID: %s
:END:
*** Function
*%s*
*** Documentation
%s\n"
      key uuid descript (replace-regexp-in-string (rx bol "*") "" doc-string)))
    uuid))



(defun doom-abcs/add-entry(key descript cmd insert)
  (let ((uuid (doom-abcs/build-entry key descript cmd)))
    (doom-abcs/add-row uuid key descript insert)))

(defun doom-abcs/add-table(key-parts)
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

(defun doom-abcs/add-row(uuid key descript insert)
  (goto-char (point-min))
  (re-search-forward "* Documentation")
  (re-search-backward "#\\+NAME:")
  (next-line)
  (goto-char(org-table-end))
  (forward-line -1)
  (if insert (org-table-insert-row))
  (org-table-goto-column 1)
  (insert (format "~%s~" key))
  (org-table-goto-column 2)
  (insert (format "[[#%s][%s]]" uuid descript))
  (org-table-align))

(defun doom-abcs/build-org-file (prefix)
  (with-temp-file "~/doom-abcs/index.org"
    (org-mode)
    (doom-abcs/init-page)
    (let ((bindings (doom-abcs/map-bindings prefix (current-buffer))))
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
                 (doom-abcs/add-table pfx)
                 (doom-abcs/add-entry key descript cmd nil)
                 pfx
                 )

             (progn
               (doom-abcs/add-entry key descript cmd t)
               acc
               ))))
       ""
       bindings))))

(doom-abcs/build-org-file (vector (string-to-char (kbd doom-leader-key))))




