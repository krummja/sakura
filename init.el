(server-start)

(setq inhibit-startup-message t)

(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq user-full-name "Jonathan Crum")
(setq user-mail-address "crumja@uga.edu")

(setq NOTEBOOK (concat (getenv "HOME") "/Notebook"))
(setq BIBLIOGRAPHY (concat (getenv "HOME") "/texmf/bibtex/bib/master.bib"))
(setq LIBRARY (concat (getenv "HOME") "/Dropbox/Library"))
(setq WORKSPACE (concat (getenv "HOME") "/Workspace"))

(require 'package)  ; Initialize package sources
(setq package-archives '(
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(push "~/.emacs.d/elisp" load-path)

(push "~/.emacs.d/external-packages" load-path)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package undo-tree)
(require 'undo-tree)
(global-undo-tree-mode)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))	; controls how quickly which-key pops up

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer sakura/leader-key-def
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (general-create-definer sakura/ctrl-c-keys
			  :prefix "C-c"))

(sakura/leader-key-def 
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package page-break-lines)
(use-package all-the-icons)
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "W-welcome to Emacs... I suppose...")
  (setq dashboard-startup-banner "~/.emacs.d/themes/sakura_logo.png"))

(column-number-mode)
(global-display-line-numbers-mode -1)
(add-hook 'prog-mode-hook 'linum-mode)

(set-face-attribute 'default nil 
		    :font "Fira Code Retina"
		    :height 120)

(set-face-attribute 'fixed-pitch nil
		    :font "Fira Code Retina"
		    :height 120)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)

(sakura/leader-key-def
  "tr" 'rainbow-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package eshell-toggle
  :bind ("C-`" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package ivy
  :diminish			; suppresses minor mode on modeline
  :bind (("C-s" . swiper)	; allows fuzzy searching within current buffer
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(sakura/leader-key-def
  "SPC" '(counsel-find-file :which-key "find file"))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?j ?k ?l)))

(sakura/leader-key-def 
  "w" '(:ignore t :which-key "windows")
  "w-" 'split-window-vertically
  "w/" 'split-window-horizontally
  "wd" 'delete-window
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wk" 'evil-window-up
  "wj" 'evil-window-down)

(sakura/leader-key-def
  "o" '(:ignore t :which-key "open")
  "of" 'make-frame)

(sakura/leader-key-def
  "b" '(:ignore t :which-key "buffer")
  "bb" '(counsel-ibuffer :which-key "switch-to-buffer")
  "bd" '(kill-buffer :which-key "kill-buffer"))

(sakura/leader-key-def
  "C-m" '(:ignore t :which-key "bookmarks")
  "C-m n" '(bookmark-set :which-key "bookmark-set")
  "C-m d" '(bookmark-delete :which-key "bookmark-delete")
  "C-m C-m" '(bookmark-bmenu-list :which-key "bookmark-list"))

(sakura/leader-key-def
  "n" '(:ignore t :which-key "notebook")
  "nb" '(:ignore t :which-key "bibtex"))

(use-package neotree
  :config
  (setq 
   neo-theme (if (display-graphic-p) 'icons 'arrow)
   neo-window-fixed-size nil
   neo-window-width 15
   )
  :init
  (add-hook 'neo-after-create-hook (lambda (&rest _) (display-line-numbers-mode -1))))

(sakura/leader-key-def
  "t`" '(neotree-toggle :which-key "neotree"))

(defun sakura/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :defer t
  :hook (org-mode . sakura/org-mode-setup)
  :config
  (setq 
   org-hide-emphasis-markers t
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-edit-src-content-indentation 0
   org-hide-block-startup nil
   org-src-preserve-indentation nil
   org-startup-folded t
   org-cycle-separator-lines 2
   org-directory NOTEBOOK
   org-return-follows-link t
   org-support-shift-select t
   org-agenda-files '("~/Notebook/index.org")
   org-refile-targets '(
			(org-agenda-files :maxlevel . 3)
			(org-agenda-files :maxlevel . 3))
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-todo-keywords '(
		       (sequence "TODO" "|" "DONE")
		       (sequence "DOING" "PAUSED" "CANCELLED")
		       (sequence "NOTE" "PROJECT" "|" "TO ARCHIVE"))
   org-capture-templates '(
			   ("n" "Note" entry (file+headline "~/Notebook/index.org" "INBOX")
			    "* NOTE  %?\n" :empty-lines 1)
			   ("t" "Teaching" entry (file+headline "~/Notebook/index.org" "TEACHING")
			    "* TODO  %?\n" :empty-lines 1)
			   ("p" "Project" entry (file+headline "~/Notebook/index.org" "PROJECTS")
			    "* TODO  %?\n" :empty-lines 1)
			   ("b" "Bib Entry" entry (file+headline "~/Notebook/index.org" "RESEARCH")
			    "*  %?\n" :empty-lines 1)
			   ("o" "Link capture" entry (file+headline "~/Notebook/index.org" "WEB BOOKMARKS")
			    "* %a %U" :immediate-finish t))
   org-protocol-default-template-key "o"
   org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup))

(sakura/leader-key-def
  "l" '(:ignore t :which-key "links")
  "ll" '(org-store-link :which-key "org-store-link")
  "li" '(org-insert-link :which-key "org-insert-link")
  "lI" '(org-insert-all-links :which-key "org-insert-all-links")
  "c"  '(:ignore t :which-key "capture")
  "cc" '(org-capture :which-key "org-capture"))

(setq org-agenda-format-date 
      (lambda (date) (concat "\n"
			     (make-string (window-width) 9472)
			     "\n"
			     (org-agenda-format-date-aligned date))))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
	 ((tags "PRIORITY=\"A\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "High-priority unfinished tasks:")))
	  (agenda "" ((org-agenda-ndays 1)))
	  (alltodo ""
		   ((org-agenda-skip-function '(or
						(sakura/org-skip-subtree-if-habit)
						(sakura/org-skip-subtree-if-priority ?A)
						(org-agenda-skip-if nil '(scheduled deadline))))
		    (org-agenda-overridding-header "\n\nALL normal priority tasks:"))))
	 ((org-agenda-block-separator "------------------------------------------")))))

(defun sakura/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
	(pri-value (* 1000 (- org-lowest-priority priority)))
	(pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
	subtree-end
      nil)))
		   
(defun sakura/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a style property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
	subtree-end
      nil)))

(sakura/leader-key-def
  "na" '(org-agenda :which-key "agenda"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("▶")))
;;  (org-bullets-bullet-list '("☰" "☷" "☵" "☲" "☳" "☴" "☶" "☱")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (dot . t)
   (python .t)))

(require 'org-indent)

(setq org-html-validation-link nil)
(require 'org-protocol)

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq org-noter-notes-window-location 'other-frame)
  (setq org-noter-always-create-frame nil)
  (setq org-noter-hide-other nil)
  (setq org-noter-notes-search-path '("~/Notebook")))

(sakura/leader-key-def 
  "nn" '(:ignore t :which-key "noter")
  "nnn" '(org-noter :which-key "org-noter")
  "nni" '(org-noter-insert-note :which-key "insert note")
  "nnI" '(org-noter-insert-precise-note :which-key "insert note precise")
  "nns" '(org-noter-sync-current-note :which-key "sync current note"))

(use-package org-ref
  :after (:any org org-noter org-roam)
  :config
  (setq reftex-default-bibliography BIBLIOGRAPHY)

  (setq org-ref-bibliography-notes "~/Notebook/index.org")
  (setq org-ref-default-bibliography '("~/texmf/bibtex/bib/master.bib"))
  (setq org-ref-pdf-directory LIBRARY)
  (setq org-ref-completion-library 'helm-bibtex)
  (setq org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
  (setq org-ref-note-title-format 
	"* TODO %y - %t\n    \
 :PROPERTIES:\n        \
 :Custom_ID: %k\n      \
 :NOTER_DOCUMENT: %F\n \
 :ROAM_KEY: cite:%k\n  \
 :AUTHOR: %9a\n        \
 :JOURNAL: %j\n        \
 :YEAR: %y\n           \
 :VOLUME: %v\n         \
 :PAGES: %p\n          \
 :DOI: %D\n            \
 :URL: %U\n            \
 :END:\n\n
"
) 
  (setq org-ref-notes-directory "~/Notebook")
  (setq org-ref-notes-function 'orb-edit-notes))

  (setq bibtex-completion-bibliography BIBLIOGRAPHY)
  (setq bibtex-completion-library-path LIBRARY)
  (setq bibtex-completion-notes-path NOTEBOOK)

(sakura/leader-key-def
  "nbb" '(helm-bibtex :which-key "helm-bibtex")
  "nbn" '(helm-bibtex-with-notes :which-key "helm-bibtex-with-notes"))

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :config
  (setq org-roam-directory NOTEBOOK)
  (setq org-roam-index-file "~/Notebook/index.org")
  (setq org-roam-completions-everywhere t)
  (setq org-roam-capture-templates 
	'(("d" "default" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "${slug}"
	   :head "#+TITLE: ${title}\n

- tags :: "
	   :unnarrowed t)))
  (setq org-roam-dailies-directory "~/Notebook/daily/")
  (setq org-roam-dailies-capture-templates
	'(("r" "research" entry
	   #'org-roam-capture--get-point
	   "*  %?"
	   :file-name "daily/%<%Y-%m-%d>"
	   :head "#+TITLE: %<%Y-%m-%d>\n"
	   :olp ("Research notes"))
	  ("j" "journal" entry
	   #'org-roam-capture--get-point
	   "*  %?"
	   :file-name "daily/%<%Y-%m-%d>"
	   :head "#+TITLE: %<%Y-%m-%d>\n"
	   :olp ("Journal"))
	  ("p" "projects" entry
	   #'org-roam-capture--get-point
	   "*  %?"
	   :file-name "daily/%<%Y-%m-%d>"
	   :head "#+TITLE: %<%Y-%m-%d>\n"
	   :olp ("Projects")))))

(defun sakura/visualize-org-roam ()
  "Either switch to the existing buffer for org-roam-server or make a new one with eaf."
  (interactive)
  (if (get-buffer "Org Roam Server")
      (switch-to-buffer "Org Roam Server")
    (eaf-open-browser "127.0.0.1:8080")))  ;; Will need to install EAF - looks very cool...

(sakura/leader-key-def
  "r"   '(:ignore t :which-key "roam")
  "rD"  '(org-roam-doctor :which-key "roam doctor")
  "rh"  '(org-roam-jump-to-index :which-key "roam home")
  "rr"  '(org-roam :which-key "org-roam")
  "rf"  '(org-roam-find-file :which-key "roam find file")
  "ru"  '(org-roam-unlinked-references :which-key "find unlinked refs")
  "rg"  '(org-roam-graph-show :which-key "roam show graph")
  "ri"  '(org-roam-insert :which-key "roam insert")
  "rI"  '(org-roam-insert-immediate :which-key "roam insert immediate"))

(sakura/leader-key-def
  "rd"  '(:ignore t :which-key "roam dailies")
  "rdd" '(org-roam-dailies-find-today :which-key "visit today")
  "rdy" '(org-roam-dailies-find-yesterday :which-key "visit yesterday")
  "rdt" '(org-roam-dailies-find-tomorrow :which-key "visit tomorrow")
  "rdD" '(org-roam-dailies-find-date :which-key "visit date")
  "rdc" '(org-roam-dailies-capture-today :which-key "capture"))

(require 'org-roam-protocol)

(use-package org-roam-server
  :ensure t
  :config
  (setq org-rome-server-host "127.0.0.1"
	org-roam-server-port 8080
	org-roam-server-authenticate nil
	org-roam-server-export-inline-images t
	org-roam-server-files nil
	org-roam-server-served-file-extensions '("pdf" "md" "tex" "bib")
	org-roam-server-network-poll t
	org-roam-server-network-arrows nil
	org-roam-server-network-label-truncate t
	org-roam-server-label-truncate-length 60
	org-roam-server-network-label-wrap-length 20))

(sakura/leader-key-def
  "nrv" '(org-roam-server-mode :which-key "visualize roam"))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
	   :head "#+TITLE: ${author-or-editor} ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

(sakura/leader-key-def
  "nba" '(orb-note-actions :which-key "orb-note-actions"))

(use-package pdf-tools
  :if (display-graphic-p)
  :mode ("\\.pdf$" . pdf-view-mode)
  :init (load "pdf-tools-autoloads" nil t)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0))))

(use-package powerthesaurus)

(sakura/leader-key-def
  "nt" '(powerthesaurus-lookup-word-dwim :which-key "powerthesaurus"))

(use-package writeroom-mode)
(setq writeroom-width 120)

(sakura/leader-key-def
  "tw" '(writeroom-mode :which-key "writeroom"))

(use-package zotxt
  :hook (after-init . org-zotxt-mode))
(sakura/leader-key-def
  "z"   '(:ignore t :which-key "zotero")
  "zi"  '(org-zotxt-insert-reference-link :which-key "insert reference")
  "zo"  '(org-zotxt-open-attachment :which-key "open attachment")
  "zu"  '(org-zotxt-update-reference-link-at-point :which-key "update reference")
  "zn"  '(org-zotxt-noter :which-key "take notes"))

(use-package projectile
  :diminish projectile-mode
  :config
  (setq org-projectile-projects-file "~/Workspace/projects.org"
	      projectile-project-search-path '("~/Workspace"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(projectile-global-mode)

(use-package counsel-projectile
  :after projectile)

(sakura/leader-key-def
  "p"  '(:ignore t :which-key "projectile")
  "pa" '(projectile-add-known-project :which-key "add project")
  "pr" '(projectile-remove-known-project :which-key "remove project")
  "pf" '(counsel-projectile-find-file :which-key "find file")
  "pp" '(projectile-switch-project :which-key "switch project")
  "ps" '(projectile-switch-open-project :which-key "switch open project")
  "pt" '(projectile-find-tag :which-key "find tag"))

(use-package yasnippet
  :ensure t
  :config
  (setq yas/indent-line 'fixed))
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(use-package deft
  :config
  (setq deft-default-extension "org")
  (setq deft-extensions '("org"))
  (setq deft-directory "~/Notebook")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules '((noslash . "-")
				       (nospace . "-")
				       (case-fn . downcase)))
  (setq deft-text-mode 'org-mode)
  (setq deft-strip-summary-regexp ".*$"))

(sakura/leader-key-def
  "sd" '(deft :which-key "deft")
  "sD" '(deft-find-file :which-key "deft-find-file"))

(use-package helm-bibtex
  :defer t
  :config 
  (setq bibtex-completion-bibliography BIBLIOGRAPHY)
  (setq bibtex-completion-library-path LIBRARY)
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-notes-path NOTEBOOK)
  (setq bibtex-completion-display-formats 
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:80} ${journal:30}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:80} Chapter ${chapter:26}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:80} ${booktitle:30}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:80} ${booktitle:30}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:80}")))
  (setq bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_KEY: cite:${=key=}\n"
         "* TODO Notes\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:")))

(use-package helm-swoop)

(sakura/leader-key-def
  "s" '(:ignore t :which-key "search")
  "ss" '(helm-swoop :which-key "swoop")
  "sa" '(helm-multi-swoop-all :which-key "swoop all the things")
  "sp" '(helm-multi-swoop-projectile :which-key "swoop project")
  "so" '(helm-multi-swoop-org :which-key "swoop org")
  "sc" '(helm-multi-swoop-current-mode :which-key "swoop same as current"))

;; Note: I need to figure out how to make C-j and C-k work in the occur buffer.

(use-package smartparens)
(smartparens-global-mode t)
(require 'smartparens-config)
(sp-local-pair 'LaTeX-mode "`" "'")
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)
(sp-pair "*" nil :actions :rem)
(sp-pair "/" nil :actions :rem)

(use-package markdown-mode
  :pin melpa-stable
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (visual-line-mode 1))

(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  (setq company-backends 
        '((company-files
           company-keywords
	   company-capf)
	   (company-abbrev company-dabbrev)
           )))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))
(setq python-shell-interpreter "python3")
(setq py-shell-name "python3")
(setq py-python-command "python3")

(use-package rust-mode)
(use-package cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(use-package racer
  :config
  (setq racer-cmd "~/.cargo/bin/racer/")
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/"))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'smartparens-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(use-package flycheck-rust)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'latex-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

(defun org-babel-tangle-collect-blocks-handle-tangle-list (&optional language tangle-file)
  "Can be used as :override advice for `org-babel-tangle-collect-blocks'.
Handles lists of :tangle files."
  (let ((counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (let ((current-heading-pos
         (org-with-wide-buffer
          (org-with-limited-levels (outline-previous-heading)))))
    (if (eq last-heading-pos current-heading-pos) (cl-incf counter)
      (setq counter 1)
      (setq last-heading-pos current-heading-pos)))
      (unless (org-in-commented-heading-p)
    (let* ((info (org-babel-get-src-block-info 'light))
           (src-lang (nth 0 info))
           (src-tfiles (cdr (assq :tangle (nth 2 info))))) ; Tobias: accept list for :tangle
      (unless (consp src-tfiles) ; Tobias: unify handling of strings and lists for :tangle
        (setq src-tfiles (list src-tfiles))) ; Tobias: unify handling
      (dolist (src-tfile src-tfiles) ; Tobias: iterate over list
        (unless (or (string= src-tfile "no")
            (and tangle-file (not (equal tangle-file src-tfile)))
            (and language (not (string= language src-lang))))
          ;; Add the spec for this block to blocks under its
          ;; language.
          (let ((by-lang (assoc src-lang blocks))
            (block (org-babel-tangle-single-block counter)))
        (setcdr (assoc :tangle (nth 4 block)) src-tfile) ; Tobias: 
        (if by-lang (setcdr by-lang (cons block (cdr by-lang)))
          (push (cons src-lang (list block)) blocks)))))))) ; Tobias: just ()
    ;; Ensure blocks are in the correct order.
    (mapcar (lambda (b) (cons (car b) (nreverse (cdr b)))) blocks)))

(defun org-babel-tangle-single-block-handle-tangle-list (oldfun block-counter &optional only-this-block)
  "Can be used as :around advice for `org-babel-tangle-single-block'.
If the :tangle header arg is a list of files. Handle all files"
  (let* ((info (org-babel-get-src-block-info))
     (params (nth 2 info))
     (tfiles (cdr (assoc :tangle params))))
    (if (null (and only-this-block (consp tfiles)))
    (funcall oldfun block-counter only-this-block)
      (cl-assert (listp tfiles) nil
         ":tangle only allows a tangle file name or a list of tangle file names")
      (let ((ret (mapcar
          (lambda (tfile)
            (let (old-get-info)
              (cl-letf* (((symbol-function 'old-get-info) (symbol-function 'org-babel-get-src-block-info))
                 ((symbol-function 'org-babel-get-src-block-info)
                  `(lambda (&rest get-info-args)
                     (let* ((info (apply 'old-get-info get-info-args))
                        (params (nth 2 info))
                        (tfile-cons (assoc :tangle params)))
                       (setcdr tfile-cons ,tfile)
                       info))))
            (funcall oldfun block-counter only-this-block))))
          tfiles)))
    (if only-this-block
        (list (cons (cl-caaar ret) (mapcar #'cadar ret)))
      ret)))))


(advice-add 'org-babel-tangle-collect-blocks :override #'org-babel-tangle-collect-blocks-handle-tangle-list)
(advice-add 'org-babel-tangle-single-block :around #'org-babel-tangle-single-block-handle-tangle-list)

(setq custom-theme-load-path '("~/.emacs.d/themes/"))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-sakura-light t)
  (doom-themes-visual-bell-config))
(require 'doom-themes)

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-themes
	'((light . doom-sakura-light)
	  (dark . doom-sakura-dark)))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook))

(sakura/leader-key-def
  "tT" '(heaven-and-hell-toggle-theme :which-key "toggle theme"))

(defvar font-lock-operator-face 'font-lock-operator-face)

(defface font-lock-operator-face
  '((((type tty) (class color)) nil)
    (((class color) (background light))
     (:foreground "dark red"))
    (t nil))
  "Used for operators."
  :group 'font-lock-faces)

(defvar font-lock-operator-keywords
  '(("\\([][|!.+=&/%*,<>(){}:^~-]+\\)" 1 font-lock-operator-face)))

(add-hook 'python-mode-hook 
	  '(lambda ()
	     (font-lock-add-keywords nil font-lock-operator-keywords t))
	  t t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
			   (0 (prog1 () 
				(compose-region (match-beginning 1) (match-end 1)"•"))))))

(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'. 
Warning: overwrites original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p))))

(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

(global-tab-line-mode)

(defun sakura/light-theme-tab-line ()
  (set-face-attribute 'tab-line nil 
		      ;; background behind tabs
		      :background "#E2D8F5"
		      :foreground "black" :distant-foreground "black"
		      :family "Fira Sans Condensed" :height 1.0 :box nil)

  (set-face-attribute 'tab-line-tab nil 
		      ;; active tab in other window
		      :inherit 'tab-line
		      :foreground "#FBF7EF" :background "#FBF7EF" :box nil)

  (set-face-attribute 'tab-line-tab-current nil 
		      ;; active tab in current window
		      :background "#FBF7EF" :foreground "#2A2A2A" :box nil)

  (set-face-attribute 'tab-line-tab-inactive nil
		      ;; inactive tab
		      :background "#E2D8F5" :foreground "#5A5A5A" :box nil)

  (set-face-attribute 'tab-line-highlight nil
		      ;; mouseover
		      :background "#ECA7D5" :foreground 'unspecified))

(defun sakura/dark-theme-tab-line ()
  (set-face-attribute 'tab-line nil 
		      ;; background behind tabs
		      :background "#2A2A2A"
		      :foreground "black" :distant-foreground "black"
		      :family "Fira Sans Condensed" :height 1.0 :box nil)

  (set-face-attribute 'tab-line-tab nil 
		      ;; active tab in other window
		      :inherit 'tab-line
		      :foreground "#2A2A2A" :background "#2A2A2A" :box nil)

  (set-face-attribute 'tab-line-tab-current nil 
		      ;; active tab in current window
		      :background "#2A2A2A" :foreground "#FBF7EF" :box nil)

  (set-face-attribute 'tab-line-tab-inactive nil
		      ;; inactive tab
		      :background "#5A5A5A" :foreground "#E2D8F5" :box nil)

  (set-face-attribute 'tab-line-highlight nil
		      ;; mouseover
		      :background "#ECA7D5" :foreground 'unspecified))

(defun sakura/tab-line-dark-theme ()
  (global-tab-line-mode)
  (sakura/dark-theme-tab-line)
  (global-tab-line-mode))

(defun sakura/tab-line-light-theme ()
  (global-tab-line-mode)
  (sakura/light-theme-tab-line)
  (global-tab-line-mode))

(sakura/tab-line-light-theme)

(sakura/leader-key-def
  "C-i"   '(:ignore t :which-key "Tab Navigation")
  "C-i j" '(tab-line-switch-to-prev-tab :which-key "Previous Tab")
  "C-i h" '(tab-line-switch-to-prev-tab :which-key "Previous Tab")
  "C-i l" '(tab-line-switch-to-next-tab :which-key "Next Tab")
  "C-i k" '(tab-line-switch-to-next-tab :which-key "Next Tab"))

(use-package powerline)
(require 'powerline)
(defvar sakura/tab-height 22)
(defvar sakura/tab-left (powerline-wave-right 'tab-line nil sakura/tab-height))
(defvar sakura/tab-right (powerline-wave-left nil 'tab-line sakura/tab-height))

(defun sakura/tab-line-tab-name-buffer (buffer &optional _buffers)
  (powerline-render (list sakura/tab-left
			  (format " %s  " (buffer-name buffer))
			  sakura/tab-right)))

(setq tab-line-tab-name-function #'sakura/tab-line-tab-name-buffer)
(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)
