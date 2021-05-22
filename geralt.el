;;; geralt.el --- A frontend to grit  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Igor Almeida

;; Author: Igor Almeida <igor.contato@gmail.com>
;; URL: https://github.com/igoralmeida/geralt
;; Version: 0.1-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines,bindings

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Grit is a command-line "personal task manager that represents tasks as nodes
;; of a multitree". Geralt is a very thin wrapper that lets you manipulate the
;; multitree from the comfort of your editor.

;;;; Installation

;;;;; Manual

;; Install these required packages:

;; + evil
;; + transient

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'geralt)

;;;; Usage

;; Run one of these commands:

;; `geralt': Open the main buffer

;;;; Tips

;; + In any geralt buffer, press `?' to see possible actions

(defvar geralt-grit-executable (executable-find "grit")
  "Absolute path to the grit executable.")

(defvar geralt-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key* 'normal map "?" #'geralt-dispatch)
    (evil-define-key* 'normal map "a" #'geralt-add)
    (evil-define-key* 'normal map "c" #'geralt-toggle-check)
    (evil-define-key* 'normal map "d" #'geralt-remove)
    (evil-define-key* 'normal map "i" #'geralt-alias)
    (evil-define-key* 'normal map "u" #'geralt-unalias)
    (evil-define-key* 'normal map "q" #'kill-current-buffer)
    (evil-define-key* 'normal map "r" #'geralt-refresh)
    (evil-define-key* 'normal map "t" #'geralt-tree)
    (evil-define-key* 'normal map "T" #'geralt-tree-new-window)
    map)
  "Keymap for geralt-mode.")

(setq geralt--geralt-mode-highlights
      '(("(\\([0-9]+\\)\\(:.*\\)?)" . font-lock-reference-face)
        ("\\[.\\]"                  . font-lock-type-face)
        ("^\\* .*"                  . font-lock-comment-face)))

(define-derived-mode geralt-mode nil
  "Geralt"
  "The mode to navigate grit buffers."
  (setq font-lock-defaults '(geralt--geralt-mode-highlights)))

(defun geralt--grit-command (outbuf &rest args)
  "Run a grit command, send the output somewhere."
  (with-temp-buffer
    (apply #'call-process geralt-grit-executable
           nil (or outbuf (current-buffer)) nil args)
    (unless outbuf
      (message (buffer-string)))))

(defun geralt--get-node-at-line ()
  "Return the node number at the current line.

I've tried, but aliases may still fool this."
  (save-excursion
    (end-of-line)
    (backward-char 1)
    (when (looking-at ")")
      (evil-jump-item)
      (let ((has-id (looking-at "(\\([0-9]+\\)\\(:.*\\)?)")))
        (when has-id
          (string-to-number (match-string-no-properties 1)))))))

;;;###autoload
(defun geralt ()
  "Open the main geralt buffer."
  (interactive)
  (if (or (not geralt-grit-executable)
          (not (file-executable-p geralt-grit-executable)))
      (message "Couldn't find/execute grit, check geralt-grit-executable")
    (let ((b (get-buffer-create "*geralt*")))
      (pop-to-buffer b)
      (geralt-refresh)
      (geralt-mode))))

(defun geralt--render-as-root (node buffer)
  "Render a node as root."
  (with-current-buffer buffer
    (geralt--grit-command buffer "tree" (format "%d" node))))

(defun geralt--get-root-from-name (buf-name)
  "Given a buffer's name, get the root node if one exists."
                                        ;TODO this logic could be a bit smarter
  (let ((idx (string-match "\\*geralt\\*<root:" buf-name)))
    (when idx
      (string-to-number (substring buf-name (match-end 0) -1)))))

(defun geralt--main (buffer)
  "Main window contents"
  (with-current-buffer buffer
    (insert "* grit\n")
    (geralt--grit-command buffer)
    (insert "* grit list\n")
    (geralt--grit-command buffer "ls")
    (insert "* grit list-dates\n")
    (geralt--grit-command buffer "lsd")))

(defun geralt-refresh (&optional buffer)
  "Refresh a geralt buffer."
  (interactive)
  (let* ((b (or buffer (current-buffer)))
         (pos (point))
         (root-maybe (geralt--get-root-from-name (buffer-name b))))
    (with-current-buffer b
      (read-only-mode -1)
      (erase-buffer)
      (if root-maybe
          (geralt--render-as-root root-maybe b)
        (geralt--main b))
      ;TODO while this "works" --- point returns to (roughly) the same place it
      ;was before the buffer was erased ---, a more intelligent way to preserve
      ;cursor position would be better
      (goto-char (point-min))
      (forward-char (- pos 1))
      (read-only-mode 1))))

(defun geralt-tree-new-window ()
  "Use a new window for `geralt-tree'."
  (interactive)
  (geralt-tree t))

(defun geralt-tree (&optional new-window)
  "Open a new buffer with the root at current node."
  (interactive "P")
  (let* ((node (geralt--get-node-at-line))
         (buffer (get-buffer-create (format "*geralt*<root:%d>" node))))
    (pop-to-buffer buffer
                   (if (not new-window)
                       (cons #'display-buffer-same-window '())
                     (cons #'display-buffer-pop-up-window '())))
    (geralt-refresh)
    (geralt-mode)))

(defun geralt--get-state-at-line ()
  "Return the completion status for the node at the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\[\\([ x*~]\\)\\]")
    (pcase (match-string 1)
      (" " 'inactive)
      ("~" 'in-progress)
      ((or "x" "*") 'completed))))

(defun geralt-toggle-check-doit (node)
  "Toggle the state of some node."
  (let ((command (pcase (geralt--get-state-at-line)
                  ((or 'inactive 'in-progress) "check")
                  ('completed "uncheck"))))
    (geralt--grit-command nil command (format "%d" node))))

(defun geralt-buffer-p (buf)
  (string-prefix-p "*geralt*" (buffer-name buf)))

(defun geralt-toggle-check ()
  "Toggle the current node state (grit affects children by default)."
  (interactive)
  (let ((node (geralt--get-node-at-line)))
    (geralt-toggle-check-doit node)
    (geralt-refresh)))

(defun geralt-add-doit (description)
  "Create a new node."
  (interactive "sDescription: ")
  (let ((switches (transient-args 'geralt-add)))
    (geralt--grit-command nil "add" (car switches) description))
  (geralt-refresh))

(transient-define-prefix geralt-add ()
  "Add a node, somewhere."
  ["Switches"
   ("-r" "Create a root node" ("-r" "--root"))
   ("-p" "Predecessor"        "--predecessor=")]
  ["Actions"
   ("a" "Add" geralt-add-doit)])

(defun geralt-rm-doit ()
  "Delete the node at line.

Note that, until recursive deletion is implemented, children
nodes will be orphaned (but grit might show them with `grit ls')."
  (interactive)
  (let* ((node-num (geralt--get-node-at-line))
         (node (format "%d" node-num)))
    (geralt--grit-command nil "rm" node)
    (geralt-refresh)))

(transient-define-prefix geralt-remove ()
  "Deletes stuff from grit."
  ["Actions"
   ("RET" "Confirm remove node" geralt-rm-doit)])

(defun geralt-alias-doit (node alias)
  "Add the ALIAS for NODE."
  (geralt--grit-command nil "alias" (format "%d" node) alias))

(defun geralt-alias (alias)
  "Create an ALIAS for the current node."
  (interactive "sAlias: ")
  (let ((node (geralt--get-node-at-line)))
    (geralt-alias-doit node alias)
    (geralt-refresh)))

(defun geralt-unalias-doit ()
  "Remove the alias from the current node."
  (interactive)
  (let ((node (geralt--get-node-at-line)))
    (geralt--grit-command nil "unalias" (format "%d" node))
    (geralt-refresh))
  )

(transient-define-prefix geralt-unalias ()
  "Remove current node's alias."
  ["Actions"
   ("RET" "Confirm unalias node" geralt-unalias-doit)])

(transient-define-prefix geralt-dispatch ()
  "List available commands."
  ["Actions"
   ("a" "Add a child (or root) node" geralt-add)
   ("c" "Check/uncheck node"         geralt-toggle-check)
   ("d" "Delete node(s)"             geralt-remove)
   ("i" "Alias"                      geralt-alias)
   ("u" "Unalias"                    geralt-unalias)
   ("r" "Refresh"                    geralt-refresh)
   ("t" "Root at node"               geralt-tree)])

(provide 'geralt)
