(add-to-list 'load-path "/home/eldar/.emacs.d/")

(when
    (load
     (expand-file-name "/home/eldar/.emacs.d/elpa/package.el"))
  (package-initialize))

(setq inhibit-splash-screen t)

(custom-set-faces
  '(default ((t (:inherit nil :stipple nil :background "white" 
     :foreground "black" :inverse-video nil
     :box nil :strike-through nil :overline nil 
     :underline nil :slant normal :weight normal
     :height 128 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
    
(custom-set-variables
    '(warning-minimum-level :error)
    '(desktop-enable t nil (desktop))
    '(cua-mode t nil (cua-base))
    '(display-time-mode t)
    '(column-number-mode t)
    '(tool-bar-mode nil))

;; Short answers    
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable splash screen and startup message    
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable backup
(setq backup-inhibited t)
;; Disable auto save
(setq auto-save-default nil)

;; Disable toolbar and menubar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Show line numbers
(global-linum-mode t)
    
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")))

(when (load (expand-file-name "/home/eldar/.emacs.d/package.el"))
    (package-initialize))

;; Must be installed color themes for GNU Emacs    
;; A dark color theme for GNU Emacs    
(require 'color-theme-actress)
(color-theme-actress)

;; Must be installed emacs integration for rvm
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;; Resource: http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/eldar/.emacs.d/ac-dict")
(ac-config-default)

;; Resource: http://cx4a.org/software/rsense/manual.html#Introduction
(setq rsense-home "/home/eldar/opt/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

;; Complete by C-c .
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-rsense)))

;; Start completion automatically after inserting . and ::
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;; Fullscreen
(defun toggle-fullscreen (&optional f)
(interactive)
(let ((current-value (frame-parameter nil 'fullscreen)))
(set-frame-parameter nil 'fullscreen
(if (equal 'fullboth current-value)
(if (boundp 'old-fullscreen) old-fullscreen nil)
(progn (setq old-fullscreen current-value)
'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)
; Make new frames fullscreen by default. Note: this hook doesn't do
; anything to the initial frame if it's in your .emacs, since that file is
; read _after_ the initial frame is created.
(add-hook 'after-make-frame-functions 'toggle-fullscreen)

            