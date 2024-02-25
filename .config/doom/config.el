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

;; Fish (and possibly other non-POSIX shells) is known to inject garbage
;;  output into some of the child processes that Emacs spawns. Many Emacs
;;  packages/utilities will choke on this output, causing unpredictable
;;  issues. To get around this, either:

;;    - Add the following to $DOOMDIR/config.el:

(setq shell-file-name (executable-find "bash"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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

;; Use packages
(use-package! lsp-tailwindcss)

;; After pkgs loaded
(after! projectile (setq projectile-project-root-files-bottom-up (remove
                                                                  ".git" projectile-project-root-files-bottom-up)))

(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;; rss feed rules
;;
;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
;;
;; (setq rmh-elfeed-org-files (list "~/.miozu/elfeed.org"));;
(setq rmh-elfeed-org-files '("~/.miozu/elfeed.org"))

;; Automatically updating feed when opening elfeed
(add-hook! 'elfeed-search-mode-hook #'elfeed-update)

;; The default filter
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))


;; Mark all YouTube entries
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

;; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "3 weeks ago"
                              :remove 'unread))

;; Configure the lsp to use your perfered formatter
;; (after! lsp-haskell
;;   (setq lsp-haskell-formatting-provider "brittany"))
;;

;; Define macroses
;; e - elfeed
(map! :leader
      :desc "start elfeed"
      :prefix "e"
      :nv "e" 'elfeed)

(map! :leader
      :desc "add elfeed feed"
      :prefix "e"
      :nv "a" 'elfeed-add-feed)

(map! :leader
      :desc "update elfeed"
      :prefix "e"
      :nv "u" 'elfeed-update)

;; d - development

(map! :leader
      :desc "console-log-macro"
      :prefix "d"
      :nv "c" (λ! (move-end-of-line nil)
                  (insert "\nconsole.log(`log:  $`, );")
                  (backward-char 7)
                  (evil-insert-state)
                  (evil-mc-make-cursor-here)
                  (evil-mc-make-and-goto-next-cursor)
                  (evil-forward-char 5)))

(map! :leader
      :desc "code-json-macro"
      :prefix "d"
      :nv "j" (λ! (insert "\n<pre>{JSON.stringify(, null, 2)}</pre>")
                  (backward-char 17)
                  (evil-insert-state)))

(map! :leader
      :desc "svelte-script-macro"
      :prefix "d"
      :nv "t" (λ! (insert "<script>")
                  (insert "\n</script>")
                  (evil-open-above 1)))

(map! :leader
      :desc "svelte-style-macro"
      :prefix "d"
      :nv "e" (λ! (insert "<style>")
                  (insert "\n</style>")
                  (evil-open-above 1)))

(map! :leader
      :desc "svelte-section-macro"
      :prefix "d"
      :nv "n" (λ! (move-end-of-line nil)
                  (insert "\n<section class=\"\">")
                  (insert "\n</section>")
                  (evil-open-above 1)))

(map! :leader
      :desc "svelte-div-macro"
      :prefix "d"
      :nv "d" (λ! (move-end-of-line nil)
                  (insert "\n<div class=\"\">")
                  (insert "\n</div>")
                  (evil-open-above 1)))

(map! :leader
      :desc "svelte-load-fn-macro"
      :prefix "d"
      :nv "l" (λ! (insert "export async function load({}) {")
                  (insert "\n}")
                  (forward-line -1)
                  (move-end-of-line nil)
                  (backward-char 4)
                  (evil-insert-state)))

(map! :leader
      :desc "svelte-multicursor-empty-block-macro"
      :prefix "d"
      :nv "b" (λ! (insert "\n{#}")
                  (insert "\n{/}")
                  (backward-char 1)
                  (evil-insert-state)
                  (evil-mc-make-cursor-here)
                  (evil-mc-make-and-goto-next-cursor)
                  (forward-line -1)
                  (evil-forward-char 2)))




;; make files open faster in fundamental mode without undo
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (linum-mode -1)
    (font-lock-mode -1)
    )
  )

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
