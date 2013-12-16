(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-in-emacs-state t)

(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("orange" bar))

(setq evilnc-hotkey-comment-operator "gc")

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-visualstar)
(require-package 'evil-nerd-commenter)
(require-package 'evil-indent-textobject)
(require-package 'evil-matchit)
(require-package 'surround)

(require 'evil)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'evil-indent-textobject)
(require 'evil-visualstar)
(require 'evil-matchit)
(require 'surround)

(global-evil-leader-mode)
(evil-mode t)
(global-surround-mode 1)

(dolist (mode '(eshell-mode
                shell-mode
                term-mode
                terminal-mode
                comint-mode
                skewer-repl-mode
                profiler-report-mode
                erc-mode weechat-mode
                direx:direx-mode
                project-explorer-mode))
  (evil-set-initial-state mode 'emacs))

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(evil-define-text-object my-evil-next-match (count &optional beg end type)
  "Select next match."
  (evil-ex-search-previous 1)
  (evil-ex-search-next count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(evil-define-text-object my-evil-previous-match (count &optional beg end type)
  "Select previous match."
  (evil-ex-search-next 1)
  (evil-ex-search-previous count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(define-key evil-motion-state-map "gn" 'my-evil-next-match)
(define-key evil-motion-state-map "gN" 'my-evil-previous-match)

(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(provide 'init-evil)
