(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(default ((t (:family "Envy Code R" :foundry "outline" :slant normal :weight normal :height 98 :width normal)))))

(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "FORMAT SUCCESSFULLY"))
(global-set-key [f7] 'indent-whole)

(setq-default indent-whole nil)

(setq backup-directory-alist(quote (("." . "~/.backups"))))

(global-linum-mode t)

(add-to-list 'load-path "~/.emacs.d/")

(require 'taglist)

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

(require 'monokai-theme)

