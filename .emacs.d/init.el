;; NOTE: you may need to run M-x package-list-packages before they can
;; be found.

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
             '("melpa" . "https://melpa.org/packages/") t)

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

    ;; Rust
    rust-mode
    racer    
    
    ;; Python
    python-mode
    jedi

    haskell-mode

    ;; Clojure
    clojure-mode
    cider

    toml-mode
    
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

;; Enable paredit.
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'haskell-mode-hook          #'enable-paredit-mode)

;; Enable rainbow delimiters.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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

;; Enable electric pair for symbol pairing.
(electric-pair-mode t)


;; Company mode.
;; Allow C-n and C-p to move around on autocomplete window.
(require 'company)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;; Rust stuff.
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Python stuff.
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

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
 '(package-selected-packages
   '(jedi use-package undo-tree toml-mode solarized-theme smex smartparens smart-mode-line ripgrep rg rainbow-delimiters racer python-mode projectile paredit magit lsp-rust lsp-python lsp-haskell ido-completing-read+ flycheck flx-ido company-lsp cider base16-theme atom-one-dark-theme ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
