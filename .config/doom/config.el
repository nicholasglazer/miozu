;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nicholas Glazer"
      user-mail-address "glazer.nicholas@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-miozu)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(after! projectile (setq projectile-project-root-files-bottom-up (remove
            ".git" projectile-project-root-files-bottom-up)))

(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))


;; define macroses
(defun my/console-log-macro ()
  "Inserts a console.log statement."
  (interactive)
  (let ((selected-text (if (evil-visual-state-p)
                           (buffer-substring-no-properties evil-visual-beginning evil-visual-end)
                         "")))
    (progn
      (end-of-line)                                         ; Move to the end of the line
      (newline-and-indent)                                  ; New line with indent
      (insert "console.log(`log: :" selected-text "${}`);") ; Insert the statement
      (end-of-line)                                         ; Move to the end of the line
      (backward-char 4)                                     ; Move forward to be after "${"
      (evil-insert-state)                                   ; Enter insert mode
      )))

(map! :leader
      :desc "Insert console.log"
      "-" #'my/console-log-macro)


;; Configure the lsp to use your perfered formatter
;; (after! lsp-haskell
;;   (setq lsp-haskell-formatting-provider "brittany"))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
