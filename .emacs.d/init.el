;; Load zshrc paths.
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))


(require 'package)

;; Trust all themes by default.
(setq custom-safe-themes t)

;; Add additional package sources.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Set priorities for packages to find.
(setq package-archive-priorities
      '(("org"          . 200)
        ("melpa-stable" . 150)
        ("melpa"        . 100)
        ("marmalade"    . 75)
        ("gnu"          . 50)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add packages we want here.
(defvar my-packages
  '(smex
    solarized-theme
    atom-one-dark-theme
    base16-theme
    flycheck
    undo-tree
    paredit
    rainbow-delimiters
    rg
    markdown-mode
    smart-mode-line
    magit
    projectile
    flx
    flx-ido
    ido-completing-read+
    company
    smartparens) 
  "A list of packages to ensure are installed at launch.")

;; Install missing packages.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Disable UI stuff.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable the annoying bell.
(setq ring-bell-function 'ignore)

;; Set default theme.
(load-theme 'solarized-light)

;; Highlight matching paren.
(show-paren-mode 1)

;; Enable rainbow delimiters.
(require 'rainbow-delimiters)
(rainbow-delimiters-mode)

;; Enable company mode.
(require 'company-lsp)
(push 'company-lsp company-backends)
(add-hook 'after-init-hook 'global-company-mode)

;; Enable paredit.
(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Enable smex.
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Enable projectile in programming projects.
(require 'projectile)
(projectile-global-mode)
(add-hook 'prog-mode-hook 'projectile-mode)

;; Enable flx-ido to fuzzy search all the places.
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; Disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(require 'ido)
(ido-mode t)
(setq ido-everywhere t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Smart line mode enable.
(setq sml/theme 'respectful)
(require 'smart-mode-line)
(sml/setup)

;; Symbol prettification.
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)
    (">="      . ?≥)))

(global-prettify-symbols-mode +1)

;; Auto generated.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (smart-mode-line use-package undo-tree smex smartparens rainbow-delimiters projectile paredit markdown-mode magit ido-completing-read+ flx-ido ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
