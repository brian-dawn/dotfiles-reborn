;; TODO: magit


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; Make mouse scrolling more comfortable.
(setq mouse-wheel-progressive-speed nil)

;; No bell.
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin enabling/configuring.
;;;;;;;;;;;;;;;;;;;;;;;;


;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-c C-g") 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dotnet stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq cider-prompt-for-symbol nil)
(setq cider-auto-test-mode t)
(setq cider-prompt-save-file-on-load 'always-save)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-mode-hook #'eldoc-mode)


(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; CUSTOM FUNCTIONS

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
;; IDO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix ido - better fuzzying for projectile and friends
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(require 'ido)
(ido-mode t)
(setq ido-everywhere t)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'elpy-enable)
(add-hook 'python-mode-hook 'elpy-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#ff004d" "#00e756" "#fff024" "#83769c" "#ff77a8" "#83769c" "#5f574f"])
 '(ansi-term-color-vector
   [unspecified "#000000" "#ff004d" "#00e756" "#fff024" "#83769c" "#ff77a8" "#83769c" "#5f574f"])
 '(custom-safe-themes
   (quote
    ("4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "aeb698d431751b18153e89b5f838fc3992433780a39a082740db216c7202a1c9" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "076a94693c0f6fa99612121c18ccb48bfbd842c05b6b9ed04b6e7e0a0f95a53e" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "daa7ac1dde9d089a998fa2f90c19354fc4ef12bcfd312aca1bcf003a7c381501" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" default)))
 '(fci-rule-color "#3E4451"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#3e4450"))))
 '(company-scrollbar-fg ((t (:background "#333842"))))
 '(company-tooltip ((t (:inherit default :background "#2c3039"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
