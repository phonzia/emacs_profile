(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (adwaita)))
 '(ecb-layout-window-sizes (quote (("leftright1" (ecb-directories-buffer-name 0.2 . 0.33) (ecb-sources-buffer-name 0.2 . 0.33) (ecb-history-buffer-name 0.2 . 0.33) (ecb-methods-buffer-name 0.25 . 0.9629629629629629)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "M+ 2c" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

(setq visible-bell t)
(setq frame-title-format "emacs@%b")
(auto-image-file-mode)

(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "FORMAT SUCCESSFULLY"))
(global-set-key [f7] 'indent-whole)

(setq-default indent-whole nil)

(setq backup-directory-alist(quote (("." . "~/.backups"))))

(global-linum-mode t)

(add-to-list 'load-path "~/.emacs.d/")

(global-set-key [f5] 'compile)

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

;;show function
(which-function-mode t)

;;show parens
(show-paren-mode t)

(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'yasnippet)
(yas-global-mode t)

;;h, cpp switch
(global-set-key (kbd "C-c a") 'ff-find-related-file) 

(require 'ecb)  
(require 'xcscope) 
;;(ecb-activate)



(global-set-key [f6] 'ecb-toggle-ecb-windows)
(setq ecb-layout-name "leftright1")  

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

(defconst user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))

(let ((include-dirs user-include-dirs))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

(add-hook 'cc-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(global-set-key [f8] 'hs-toggle-hiding)

;;tab stop
(setq default-tab-width 2)

;;expand tabs
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
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.01)

;;copy n lines
(defun my-copy-lines(num)
  (point-to-register 0)
  (beginning-of-line)
  (let ((begin-point (point)))
    (forward-line num)
    (copy-region-as-kill begin-point (point)))
  (jump-to-register 0)) 
(defun my-copy-by-line(n)
  (interactive "p")
  (my-copy-lines n)
  (next-line n)
  )
(global-set-key (kbd "C-c w") 'my-copy-by-line)

;;org gtd
;;;agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list "~/KuaiPan/GTD/task.org"))
(setq org-todo-keywords '((type "TODO" "STARTED" "WAITING" "|" "DONE" "CANCELLED")))
(setq org-log-done 'time)
(setq org-log-done 'note)

;;w3m
(require 'w3m)
(setq w3m-default-display-inline-images t)

;;markdown
(require 'markdown-mode)

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
                                
(setq split-width-threshold 200)

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
(helm-mode t)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
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
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
