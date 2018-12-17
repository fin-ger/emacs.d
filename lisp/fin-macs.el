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
                         ace-jump-mode
                         auto-complete
                         auto-complete-c-headers
                         cargo
                         company
                         dockerfile-mode
                         editorconfig
                         editorconfig-charset-extras
                         editorconfig-custom-majormode
                         editorconfig-domain-specific
                         elixir-mode
                         epc
                         epl
                         flycheck-rust
                         glsl-mode
                         go-autocomplete
                         go-mode
                         iedit
                         jedi
                         json-mode
                         json-reformat
                         json-snatcher
                         less-css-mode
                         multiple-cursors
                         neotree
                         racer
                         rust-mode
                         seq
                         smooth-scrolling
                         sublime-themes
                         toml
                         toml-mode
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

;; install packages from var local-packages that are not installed on current system
(let ((need-to-install
       (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

(add-to-list 'load-path "~/.emacs.d/lisp/")




;;;;;;;;;;;;;;;;;;;
;; INIT PACKAGES ;;
;;;;;;;;;;;;;;;;;;;
;; configure editorconfig
(require 'editorconfig)
(editorconfig-mode 1)
(add-hook 'editorconfig-custom-hooks
          'editorconfig-charset-extras)
(add-hook 'editorconfig-custom-hooks
          'editorconfig-custom-majormode)
(add-hook 'editorconfig-custom-hooks
          'editorconfig-domain-specific)
;; start auto-complete with emacs
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; setup wgrep
(require 'wgrep)
(setq wgrep-change-readonly-file t)
;; HTCPCP protocol
(load "coffee.el")
;; Fix a bug in emacs' current css indentation logic
(load "css-mode.el")
(require 'smooth-scrolling)

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 5))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq-default indent-tabs-mode nil) ;; all tabs are spaces
(setq visible-bell t)

(setq frame-title-format "FinMacs - %b")

;; show up auto-complete menu immediately
(setq ac-show-menu-immediately-on-auto-complete t)
;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)
;; setup jedi
(require 'jedi)
;; install jedi dependencies
(jedi:install-server)
;; hook up to auto-complete
(add-to-list 'ac-sources 'ac-source-jedi-direct)

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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'twilight-bright t)

(defun dark-theme ()
  (interactive)
  (disable-theme 'twilight-bright)
  (load-theme 'wombat-fin t))

(defun bright-theme ()
  (interactive)
  (disable-theme 'wombat-fin)
  (load-theme 'twilight-bright t))

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
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

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

;; rust configuration
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "~/.rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; visual-line-mode for text buffers
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; add hunspell to ispell instead of aspell
(when (executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (setq ispell-dictionary "en_US"))

;; enable flyspell for all text modes
(add-hook 'text-mode-hook 'flyspell-mode)

;; go setup
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET EMACS CUSTOMIZATION VARIABLES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "4d99f0431c0882b1fdd483f30a1853e852cf448730b12ddf087a42102c08dbc0" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(electric-indent-mode t)
; '(electric-layout-mode t)
 '(fci-rule-character-color "#d9d9d9")
 '(fill-column 120)
 '(inhibit-startup-screen t)
 '(js-flat-functions t)
 '(json-reformat:indent-width 2)
 '(markdown-command "kramdown")
 '(menu-bar-mode nil)
 '(neo-hidden-regexp-list
   (quote
    ("\\.pyc$" "~$" "^#.*#$" "\\.elc$" "__.*cache.*" "\\.git$")))
 '(neo-show-hidden-files nil)
 '(neo-theme (quote arrow))
 '(neo-window-fixed-size nil)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   (quote
    (jsx-mode select-script-mode vue-mode elm-mode markdown-mode markdown-mode+ markdown-preview-eww markdown-preview-mode markdown-toc markdownfmt yaml-mode yasnippet wgrep web-mode undo-tree typescript-mode twilight-bright-theme tss sublime-themes smooth-scrolling seq neotree multiple-cursors jedi iedit glsl-mode epl auto-complete-c-headers ace-jump-mode)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(semantic-c-dependency-system-include-path
   (quote
    ("/usr/include" "/usr/lib/gcc/x86_64-pc-linux-gnu/4.7.3/include/g++-v4")))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vhdl-basic-offset 4))
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




;;;;;;;;;;;;;;;;;;;;;
;; NEOTREE SORTING ;;
;;;;;;;;;;;;;;;;;;;;;
(setq neo-window-width 30)
(defadvice neo-buffer--get-nodes
    (after neo-buffer--get-nodes-new-sorter activate)
  (setq ad-return-value
        (let ((nodes ad-return-value)
              (comparator (lambda (s1 s2) (string< (downcase s1)
                                                   (downcase s2)))))
          (apply 'cons (mapcar (lambda (x) (sort (apply x (list nodes))
                                                 comparator))
                               '(car cdr))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENABLE C-x w h AND C-x w r ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hi-lock-mode 1)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENABLE LINE NUMBER BAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-linum-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHLIGHT CURRENT LINE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode)




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
(global-set-key (kbd "C-X C-D") 'neotree-dir)
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
