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



(defun build-entry (key descript cmd)
      (format
                     "** %s \n*** Function\n %s\n*** Documentation\n\t%s\n" key descript (documentation cmd)))

(defun build-org-file (prefix buffer)
  (with-current-buffer "*scratch*"
    (erase-buffer)
    (org-table-create)
    (let ((bindings (map-bindings prefix buffer)))
      (-reduce-from
       (lambda (acc it)
         (let* ((keymap (car it))
                (cmd (cdr it))
                (key (car keymap))
                (keyparts (split-string key " "))
                (pfx (nth 1 keyparts))
                (descript (cdr keymap)))
           (if (not (equal acc pfx))
               (progn
                 (insert (format "* %s \n" (string-join (-take 2 keyparts))))
                 (insert (build-entry key descript cmd))
                 pfx
                 )

             (progn
               (insert (build-entry key descript cmd))
               acc
               ))))
       ""
       bindings))))

(build-org-file (vector (string-to-char (kbd doom-leader-key))) (current-buffer))