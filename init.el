(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defconst demo-packages
  '(xcscope
    ecb
    ggtags
    helm
    helm-gtags
    function-args
    yasnippet
    smartparens
    molokai-theme
    ace-jump-mode
    evil
    auto-complete
    powerline
    switch-window
    zenburn-theme
    undo-tree
    magit))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(global-set-key (kbd "RET") 'newline-and-indent)

;;highlight lines longer than 80 chars
(setq whitespace-line-column 80)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode)

;;code indent
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
(global-set-key [f7] 'indent-whole)

;;tab stop
(setq default-tab-width 2)
;;expand tabs
(setq-default indent-tabs-mode  nil)

;;set backup dir
(setq backup-directory-alist (quote (("." . "~/.backups"))))

;;show line number
(global-linum-mode t)

;;show parens
(show-paren-mode t)

;;show function
(which-function-mode t)

;;compile
(defun ph-compile (&optional cmd)
  (interactive "sMake args:")
  (if (= (length cmd) 0)
      (compile (concat "make -j4"))
    (compile (concat "make " cmd))
    )
  )
(global-set-key [f5] 'ph-compile)

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

;;yasnippet
(require 'yasnippet)
(yas-global-mode t)

;;h, cpp switch
(global-set-key (kbd "C-c a") 'ff-find-related-file)
;;xcscope
(require 'xcscope)
(setq cscope-allow-arrow-overlays nil)

;;ecb
(require 'ecb)
(global-set-key [f6] 'ecb-toggle-ecb-windows)
(setq ecb-layout-name "left9")
(ecb-activate)
(ecb-toggle-ecb-windows)

;;add *.h files to c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;semantic
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(set-default 'semantic-case-fold t)
(semantic-mode 1)
(defconst user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public" "../gen-cpp" "../../gen-cpp"
        "../.." "../../include" "../../inc" "../../common" "../../public" "../../.."))
(let ((include-dirs user-include-dirs))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))
(global-set-key (kbd "C-c ]")
                (lambda ()
                  (interactive)
                  (bookmark-set "jump")
                  (semantic-ia-fast-jump (point))
                  ))
(global-set-key (kbd "C-c m") 'bookmark-set)
(global-set-key (kbd "C-c b") (lambda()
                                (interactive)
                                (bookmark-jump "jump")))

;;code folding
(add-hook 'cc-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(global-set-key [f8] 'hs-toggle-hiding)

;;indent style
(setq-default indent-tabs-mode  nil)
(defun vlad-cc-style()
  (c-set-style "linux")
  (c-set-offset 'innamespace '0)
  (c-set-offset 'inextern-lang '0)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'label '*)
  (c-set-offset 'case-label '*)
  (c-set-offset 'access-label '/)
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  )
(add-hook 'c++-mode-hook 'vlad-cc-style)

;;auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-fuzzy-enable t)
(setq ac-quick-help-delay 1.0)
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.01)
(setq ac-dwim t)
(global-auto-complete-mode t)

;;copy n lines
(defun ph-copy-lines(num)
  (point-to-register 0)
  (beginning-of-line)
  (let ((begin-point (point)))
    (forward-line num)
    (copy-region-as-kill begin-point (point)))
  (jump-to-register 0))
(defun ph-copy-by-line(n)
  (interactive "p")
  (ph-copy-lines n)
  (next-line n)
  )
(global-set-key (kbd "C-c w") 'ph-copy-by-line)

;;pairs
;;(electric-pair-mode t)

;;evil
(global-set-key (kbd "C-c z") 'evil-mode)

;;advoid window split
(setq split-width-threshold 100)

;;powerline
(require 'powerline)
(powerline-default-theme)
(defun graphic-powerline-config ()
  "powerline setting for graphic"
  (interactive)
  (progn
    (setq powerline-arrow-shape 'arrow)
    (custom-set-faces
     '(mode-line ((t (:foreground "white" :background "#0044cc" :box nil))))
     '(mode-line-inactive ((t (:foreground "white" :background "#262626" :box nil))))
     )
    (setq powerline-color1 "#0088cc")
    (setq powerline-color2 "white")
    ))
(graphic-powerline-config)

;;helm
(require 'helm-config)
(require 'helm-gtags)
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-semantic-or-imenu)
(helm-mode t)

;;ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g y") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(require 'function-args)
(fa-config-default)

(global-undo-tree-mode t)

(defun ph-complete-file-name ()
  (interactive)
  (comint-dynamic-list-filename-completions)
  (comint-dynamic-complete-as-filename))
(global-set-key (kbd "C-c c") 'ph-complete-file-name)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "C-x o") 'switch-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
