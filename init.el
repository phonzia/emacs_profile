;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defconst demo-packages
  '(ggtags
    helm
    helm-gtags
    yasnippet
    smartparens
    molokai-theme
    ace-jump-mode
    auto-complete
    powerline
    markdown-mode
    thrift
    go
    undo-tree
    tango-plus-theme
    magit
    magit-svn
    function-args
    ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; config
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(server-start)

(global-set-key (kbd "RET") 'newline-and-indent)
;;tab stop
(setq default-tab-width 4)
(setq tab-width 4)
;;expand tabs
(setq-default indent-tabs-mode  nil)

;;set backup dir
(setq backup-directory-alist (quote (("." . "~/.backups"))))
(setq auto-save-default nil)

;;show line number
(global-linum-mode t)

;;show parens
(show-paren-mode t)

;;show function
(which-function-mode t)

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

;;h, cpp switch
(global-set-key (kbd "C-c a") 'ff-find-related-file)

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
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  )
(add-hook 'c++-mode-hook 'vlad-cc-style)

;;semantic
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(set-default 'semantic-case-fold t)
(semantic-mode 1)
(defconst user-include-dirs
  (list "../services_sdk_new/include" "../services_sdk_new/adapter" "../services_sdk_new/server" "../services_sdk_new" "../../protocol" "../../core" "../../core/sox" "../../core/corelib" "../../common" "../../" "../../common/core" "../../common/occi" "../../server_common/helper" "../thrift0.6.1" "../vip2_dao" "../vip_pk_thrift_client_lib/gen-cpp" "../../server_common/" "../"))
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

;;bookmark
(global-set-key (kbd "C-c m") 'bookmark-set)
(global-set-key (kbd "C-c b") (lambda()
                                (interactive)
                                (bookmark-jump "jump")))

;;code folding
(add-hook 'cc-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(global-set-key [f8] 'hs-toggle-hiding)

;;add *.h files to c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;pairs
(electric-pair-mode t)

;;advoid window split
(setq split-width-threshold 100)

;;undo tree
(global-undo-tree-mode t)

;;hippie-expand
(global-set-key (kbd "C-c f") 'hippie-expand)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; config end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; custom function
;;code indent
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
(global-set-key [f7] 'indent-whole)

;;split and find another file
(defun asplit()
  (interactive)
  (split-window-vertically)
  (ff-find-related-file)
  )
(global-set-key [f9] 'asplit)

;;compile
(defun ph-compile (&optional cmd)
  (interactive "sMake args:")
  (if (= (length cmd) 0)
      (compile (concat "make -j4"))
    (compile (concat "make " cmd))
    )
  )
(global-set-key [f5] 'ph-compile)

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
  (next-line (- n 1))
  )
(global-set-key (kbd "C-c w") 'ph-copy-by-line)

;; file name complete
(defun ph-complete-file-name ()
  (interactive)
  (comint-dynamic-list-filename-completions)
  (comint-dynamic-complete-as-filename))
(global-set-key (kbd "C-c c") 'ph-complete-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; custom function end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; plugin setting
;;yasnippet
(require 'yasnippet)
(yas-global-mode t)

;;auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-fuzzy-enable t)
(setq ac-quick-help-delay 1.0)
(setq ac-delay 0.5)
(setq ac-auto-show-menu 0.5)
(setq ac-dwim t)
(global-auto-complete-mode t)
(require 'auto-complete-clang)
(setq ac-clang-auto-save t)
(setq ac-auto-start t)
(setq ac-quick-help-delay 0.5)
(ac-set-trigger-key "TAB")
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq ac-clang-flags
        (mapcar(lambda (item)(concat "-I" item))
               (split-string
                "
 /usr/include/c++/4.3
 /usr/include/c++/4.3/x86_64-linux-gnu
 /usr/include/c++/4.3/backward
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.3.3/include
 /usr/lib/gcc/x86_64-linux-gnu/4.3.3/include-fixed
 /usr/include
../
../../
 ")))
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

;;powerline
(require 'powerline)
(powerline-default-theme)

;;helm
(require 'helm-config)
(require 'helm-gtags)
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t
      )
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)

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

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;magit
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;plugin setting end

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (ecb magit-svn go-mode go yasnippet undo-tree thrift tango-plus-theme smartparens powerline molokai-theme markdown-mode magit helm-gtags ggtags function-args auto-complete-clang ace-jump-mode)))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "nil" :slant normal :weight normal :height 120 :width normal)))))
