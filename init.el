(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defconst demo-packages
  '(ecb
    ggtags
    helm
    helm-gtags
    function-args
    yasnippet
    smartparens
    molokai-theme
    ace-jump-mode
    auto-complete
    powerline
    switch-window
    zenburn-theme
    thrift
    undo-tree
    evil
    solarized-theme
    tango-plus-theme
    auto-complete-clang
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; config
(global-set-key (kbd "RET") 'newline-and-indent)
;;tab stop
(setq default-tab-width 4)
(setq tab-width 4)
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

;;ecb
(require 'ecb)
(global-set-key [f6] 'ecb-toggle-ecb-windows)
;;(ecb-activate)

;;auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-fuzzy-enable t)
(setq ac-quick-help-delay 1.0)
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.01)
(setq ac-dwim t)
(global-auto-complete-mode t)
(require 'auto-complete-clang)
(setq ac-clang-auto-save t)
(setq ac-auto-start t)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
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
 /usr/include/mysql
/home/yangfengjia/project/diamond/vip_lib
/home/yangfengjia/project/diamond/vip2_dao
/home/yangfengjia/project/diamond/dao_rdf
/home/yangfengjia/project/common
/home/yangfengjia/project/common
/home/yangfengjia/project/protocol
/home/yangfengjia/project/common/occi
/home/yangfengjia/project/server_common
/home/yangfengjia/project/diamond/thrift0.6.1
/home/yangfengjia/project/
../
../../
./gen-cpp
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
(global-set-key (kbd "C-x o") 'switch-window)

;;magit
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;plugin setting end

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "3c093ea152d7185cc78b61b05e52648c6d2fb0d8579c2119d775630fa459e0be" default)))
 '(ecb-layout-window-sizes
   (quote
    (("leftright2"
      (ecb-directories-buffer-name 0.18888888888888888 . 0.6304347826086957)
      (ecb-sources-buffer-name 0.18888888888888888 . 0.32608695652173914)
      (ecb-methods-buffer-name 0.1962962962962963 . 0.6304347826086957)
      (ecb-history-buffer-name 0.1962962962962963 . 0.32608695652173914))
     ("left8"
      (ecb-directories-buffer-name 0.21851851851851853 . 0.2826086956521739)
      (ecb-sources-buffer-name 0.21851851851851853 . 0.21739130434782608)
      (ecb-methods-buffer-name 0.21851851851851853 . 0.2826086956521739)
      (ecb-history-buffer-name 0.21851851851851853 . 0.17391304347826086)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
