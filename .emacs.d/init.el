;; TODO: magit

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(add-to-list 'exec-path "/usr/local/bin")

;; Put all backup files in this directory.
(setq backup-directory-alist `(("." .  "~/.saves")))

(global-auto-revert-mode t)

;; Autocomplete super fast.
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;

;; Color themes.
(load-theme 'atom-one-dark t)

;; Focus follows mouse.
(setq mouse-autoselect-window t)

;; Remote the GUI stuff.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; highlight matching paren
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin enabling/configuring.
;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix ido - better fuzzying for projectile and friends
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-c C-g") 'ace-window)

;; Clojure stuff
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (rainbow-delimiters-mode 1)))

(add-hook 'cider-mode-hook
           (lambda ()
             (eldoc-mode 1)
             (rainbow-delimiters-mode 1)))

;; Clojure/Hoplon
(add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode))
;; Have cider always save a file when it's loaded in the repl.
(setq cider-prompt-save-file-on-load 'always-save)


;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (paredit-mode 1)))

(add-hook 'cider-mode-hook
           (lambda ()
             (eldoc-mode 1)
             (paredit-mode 1)))

;; For being inside tmux or terminal.
(global-set-key (kbd "C-c C-s") 'paredit-forward-slurp-sexp)

(setq cider-repl-use-pretty-printing t)
(setq cider-auto-test-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO I don't want to have to select the word :P
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'projectile)
(projectile-global-mode)
(add-hook 'prog-mode-hook 'projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Lets fix the default colors for company mode...
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)


;; Haskell stuff
(add-hook 'haskell-mode-hook 'intero-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spacemacs//cider-eval-in-repl-no-focus (form)
   "Insert FORM in the REPL buffer and eval it."
   (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
   (with-current-buffer (cider-current-repl-buffer)
    (let ((pt-max (point-max)))
      (goto-char pt-max)
      (insert form)
      (indent-region pt-max (point))
      (cider-repl-return))))


(defun spacemacs/cider-send-function-to-repl ()
  "Send current function to REPL and evaluate it without changing
the focus."
  (interactive)
  (spacemacs//cider-eval-in-repl-no-focus (cider-defun-at-point)))

(eval-after-load 'cider
  '(define-key cider-mode-map
     (kbd "C-c C-f")
     'spacemacs/cider-send-function-to-repl))
