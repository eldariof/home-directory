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

;; An on-the-fly syntax checker for GNU Emacs
(require 'flymake)

;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))
	     
(defun flymake-erb-init ()
  (let* ((check-buffer (current-buffer))
         (temp-file (flymake-create-temp-inplace (buffer-file-name) "flymake"))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (save-excursion
      (save-restriction
        (widen)
        (with-temp-file temp-file 
          (let ((temp-buffer (current-buffer)))
            (set-buffer check-buffer)
            (call-process-region (point-min) (point-max) "erb" nil temp-buffer nil "-x"))))
      (setq flymake-temp-source-file-name temp-file)
      (list "ruby" (list "-c" local-file)))))

(eval-after-load "flymake"
  '(progn
     (push '(".+\\.\\(rhtml\\|erb\\)$" flymake-erb-init) flymake-allowed-file-name-masks)
     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)))

(defun turn-on-flymake-for-erb-files ()
  (when (string-match "\.erb$" (buffer-file-name))
    (flymake-mode 1)))
(add-hook 'find-file-hook 'turn-on-flymake-for-erb-files)

(defun my-flymake-show-help () 
	(interactive)
	(when (get-char-property (point) 'flymake-overlay) 
		(let ((help (get-char-property (point) 'help-echo))) 
			(if help (message "%s" help)))))

;; Mode for automatic insertion of end blocks for Ruby			
(ruby-end-mode t)
			