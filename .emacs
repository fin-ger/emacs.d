;;;;;;;;;;;;;;;;;
;; SETUP MELPA ;;
;;;;;;;;;;;;;;;;;
;; start package.el with emacs
(require 'package)
;; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; initialize package.el
(package-initialize)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALL REQUIRED PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar local-packages '(
                         ac-js2
                         ace-jump-mode
                         auto-complete
                         auto-complete-c-headers
                         csharp-mode
                         epc
                         epl
                         fill-column-indicator
                         flymake-css
                         flymake-cursor
                         flymake-less
                         flymake-python-pyflakes
                         flymake-sass
                         flymake-vala
                         glsl-mode
                         iedit
                         jedi
                         jedi-direx
                         js2-mode
                         js2-refactor
                         json-mode
                         json-reformat
                         json-snatcher
                         multiple-cursors
                         neotree
                         omnisharp
                         projectile
                         smooth-scrolling
                         sublime-themes
                         tss
                         twilight-bright-theme
                         typescript-mode
                         undo-tree
                         web-mode
                         wgrep
                         yasnippet
                         ))
(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p)
		  (if (package-installed-p p nil) nil p))
		packages)))
(let ((need-to-install
       (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

(add-to-list 'load-path "~fin/.emacs.d/lisp/")




;;;;;;;;;;;;;;;;;;;
;; INIT PACKAGES ;;
;;;;;;;;;;;;;;;;;;;
(setq exec-path (append exec-path '("/home/fin/Entwicklung/bin")))
;; start auto-complete with emacs
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; setup wgrep
(require 'wgrep)
(setq wgrep-change-readonly-file t)
;; configure csharp auto-completion
(require 'omnisharp)
;; case converter
(load "change_case.el")
;; HTCPCP protocol
(load "coffee.el")
(load "css-mode.el")
(require 'smooth-scrolling)

;;(setq initial-frame-alist '((width . 151) (height . 74)))
;;(setq default-frame-alist '((width . 149) (height . 72)))
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 5))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq-default indent-tabs-mode nil) ;; all tabs are spaces

(defun omnisharp:get-projects-solution-file ()
  (interactive)
  ;; get directory containing current file
  (defvar sharp-project-dir
    (file-name-directory
     (expand-file-name
      (buffer-file-name (current-buffer)))))

  ;; get the parent directory of a given one
  (defun parent-directory (dir)
    (unless (equal "/" dir)
      (file-name-directory (directory-file-name dir))))

  ;; while (!(sharp-project-dir+"*.sln").exists?)
  (while (not (file-expand-wildcards (concat sharp-project-dir "*.sln")))
    (setq sharp-project-dir (parent-directory sharp-project-dir)))

  ;; return found solution file path
  (car (file-expand-wildcards (concat sharp-project-dir "*.sln"))))

(defun omnisharp:setup ()
  (local-set-key [C-tab] 'omnisharp-auto-complete)
  (setq omnisharp-server-executable-path
	"/home/fin/Entwicklung/bin/OmniSharp/OmniSharp.exe")
  (omnisharp-start-omnisharp-server (omnisharp:get-projects-solution-file))
  (auto-complete-mode)
  (flycheck-mode)
  (omnisharp-mode)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'csharp-mode-hook 'omnisharp:setup)

;; show up auto-complete menu immediately
(setq ac-show-menu-immediately-on-auto-complete t)
;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)
;; setup projectile
(require 'projectile)
(projectile-global-mode)
;; setup jedi
(require 'jedi)
;; install jedi dependencies
;(jedi:install-server)
;; hook up to auto-complete
(add-to-list 'ac-sources 'ac-source-jedi-direct)

(add-to-list 'load-path "~fin/.emacs.d/ajc-java-complete/")
(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; ### setup python ide ###
;; setup virtualenv
(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")
;; variables to help find the project root
(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")
;; function to find project root given a buffer
(defun get-project-root (buf repo-type init-file)
  (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))
(defvar jedi-config:find-root-function 'get-project-root)
;; and call this on initialization
(defun current-buffer-project-root ()
  (funcall jedi-config:find-root-function
	   (current-buffer)
	   jedi-config:vcs-root-sentinel
	   jedi-config:python-module-sentinel))
;; set the jedi server args
(defun jedi-config:setup-server-args ()
  ;; little helper macro
  (defmacro add-args (arg-list arg-name arg-value)
    '(setq ,arg-list (append ,arg-list  (list ,arg-name ,arg-value))))

  (let ((project-root (current-buffer-project-root)))
    ;; variable for this buffer only
    (make-local-variable 'jedi:server-args)

    ;; set out variable
    (when project-root
      (add-args jedi:server-args "--sys-path" project-root))
    (when jedi-config:with-virtualenv
      (add-args jedi:server-args "--virtual-env"
		jedi-config:with-virtualenv)))
  (jedi-mode)
  (jedi:setup))
(add-hook 'python-mode-hook
	  ;; 'jedi-config:setup-server-args)
	  'jedi:setup)
(setq jedi:complete-on-dot t)

;;pdb setup, note the python version
(setq pdb-path '/usr/lib/python3.4/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
	 		    (file-name-nondirectory buffer-file-name)))))

;; set theme
(add-to-list 'custom-theme-load-path "~fin/.emacs.d/themes")
(load-theme 'twilight-bright t)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;; If use bundled typescript.el,
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key binding
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
(tss-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET EMACS CUSTOMIZATION VARIABLES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "4d99f0431c0882b1fdd483f30a1853e852cf448730b12ddf087a42102c08dbc0" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(ede-project-directories
   (quote
    ("/home/fin/Entwicklung/cpp/src/cpp14_u11_codevorgabe")))
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(fill-column 120)
 '(inhibit-startup-screen t)
 '(js-flat-functions t)
 '(js2-highlight-level 3)
 '(markdown-command "kramdown")
 '(menu-bar-mode nil)
 '(neo-hidden-regexp-list
   (quote
    ("\\.pyc$" "~$" "^#.*#$" "\\.elc$" "__.*cache.*" "\\.git$")))
 '(neo-show-hidden-files nil)
 '(neo-theme (quote arrow))
 '(nxml-slash-auto-complete-flag t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(scala-indent:step 4)
 '(semantic-c-dependency-system-include-path
   (quote
    ("/usr/include" "/usr/lib/gcc/x86_64-pc-linux-gnu/4.7.3/include/g++-v4")))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vera-basic-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "unknown" :slant normal :weight normal :height 104 :width normal))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-keyword-face))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-variable-name-face)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGURE AUTO-COMPLETE FOR C/C++ DEVELOPMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let's define a function which initializes auto-complete-c-headers and gets
;; called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories
	       '"/usr/lib/gcc/x86_64-pc-linux-gnu/4.7.3/include/g++-v4"))

;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET IEDIT KEYBINDING [RENAME VARIABLE OR FUNCTION IN WHOLE PROJECT] ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set iedit keybinding
(define-key global-map (kbd "<f2>") 'iedit-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGUARE AND ADD SEMANTIC-MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on semantic mode
(semantic-mode 1)

;; turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)

;; define a function which adds semantic as a suggestion backend to auto-complete
;; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENABLE C-x w h AND C-x w r ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hi-lock-mode 1)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENABLE LINE NUMBER BAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-linum-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD A GLOBAL RULER TO INDICATE THE 80 CO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'fill-column-indicator)
(add-hook 'prog-mode-hook '(lambda () (fci-mode)))
(add-hook 'text-mode-hook '(lambda () (fci-mode)))
(setq fci-rule-width 2)
(setq fci-rule-color "#eee")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHLIGHT CURRENT LINE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode)
;;(set-face-background 'hl-line "white")
;;(set-face-foreground 'hl-line "black")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOVE TRAILING WHITESPACES ON SAVE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun custom-delete-trailing-whitespaces ()
  (when (not (and (stringp buffer-file-name)
		  (string-match "\\.md\\'" buffer-file-name)))
    (delete-trailing-whitespace)))

;; delete trailing whitespaces on save
(add-hook 'before-save-hook 'custom-delete-trailing-whitespaces)




;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "S-<f1>") 'comment-region)
(global-set-key (kbd "S-<f2>") 'uncomment-region)
(global-set-key (kbd "<f9>") 'comment-box)
(global-set-key (kbd "S-<home>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<end>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<next>") 'shrink-window) ;; Shift-PageDown
(global-set-key (kbd "S-<prior>") 'enlarge-window) ;; Shift-PageUp
(windmove-default-keybindings) ;; Shift-(left,right,up,down) to move window




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MULTIPLE CURSORS-MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)




;;;;;;;;;;;;;;;;;;;
;; ACE JUMP-MODE ;;
;;;;;;;;;;;;;;;;;;;
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)





;;;;;;;;;;;;;;;;;;;
;; SET C++ STYLE ;;
;;;;;;;;;;;;;;;;;;;
(c-add-style "fin"
	     '("gnu"
	       (c-basic-offset . 4)	; Guessed value
	       (c-offsets-alist
		(block-close . 0)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(defun-close . 0)	; Guessed value
		(defun-open . 0)	; Guessed value
		(else-clause . 0)	; Guessed value
		(statement . 0)		; Guessed value
		(statement-block-intro . +) ; Guessed value
		(substatement . +)	; Guessed value
		(substatement-open . 0)	; Guessed value
		(topmost-intro . 0)	; Guessed value
		(access-label . -)
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont c-lineup-gcc-asm-reg 0)
		(arglist-cont-nonempty . c-lineup-arglist)
		(arglist-intro . +)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro . +)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(class-close . 0)
		(class-open . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(do-while-closure . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(inclass . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . +)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . c-lineup-inexpr-block)
		(inline-close . 0)
		(inline-open . +)
		(inmodule . +)
		(innamespace . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . +)
		(label . 2)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(namespace-close . 0)
		(namespace-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-intro . +)
		(statement-case-open . 0)
		(statement-cont . +)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement-label . 2)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont . c-lineup-topmost-intro-cont))))

(defun my:c++-mode-hook ()
  (c-set-style "fin")
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my:c++-mode-hook)
(put 'upcase-region 'disabled nil)
