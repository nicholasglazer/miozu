;;; doom-miozu-theme.el --- doom-one-theme.el was used as a syntax template -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 10, 2023
;; Author: Nicholas Glazer <https://github.com/nicholasglazer>
;; Maintainer: Nicholas Glazer <https://github.com/nicholasglazer>
;; Source: https://github.com/miozutheme/doom-miozu
;;
;;; Commentary:
;;
;; To achieve harmony, colors were selected using Newton's color wheel, which should reduce eye strain
;; You can read more about it at https://miozu.com/colors
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-miozu-theme nil
  "Options for the `doom-miozu' theme."
  :group 'doom-themes)

(defcustom doom-miozu-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-miozu-theme
  :type 'boolean)

(defcustom doom-miozu-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-miozu-theme
  :type 'boolean)

(defcustom doom-miozu-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-miozu-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-miozu
  "A dark theme inspired by Atom One Dark."

  ;; name        default   256       16
  ((bg         '("#2C3040" "black"   "black"         ))
   (fg         '("#D0D2DB" "#bfbfbf" "brightwhite"   ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#232733" "#282c34" "black"         ))
   (fg-alt     '("#737E99" "#bbc2cf" "white"         ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("black"   "black"   "black"         )) ;; -//-
   (base1      '("#232733" "#1e1e1e" "brightblack"   )) ;; Miozu00
   (base2      '("#2C3040" "#2e2e2e" "brightblack"   )) ;; Miozu01
   (base3      '("#3E4359" "#262626" "brightblack"   )) ;; Miozu02
   (base4      '("#565E78" "#3f3f3f" "brightblack"   )) ;; Miozu03
   (base5      '("#737E99" "#525252" "brightblack"   )) ;; Miozu04
   (base6      '("#D0D2DB" "#6b6b6b" "brightblack"   )) ;; Miozu05
   (base7      '("#F3F4F7" "#979797" "brightblack"   )) ;; Miozu06
   (base8      '("#FAFDFB" "#dfdfdf" "white"         )) ;; Miozu07

   (grey       base4)
   (red        '("#EB3137" "#ff6655" "red"           )) ;; Miozu14
   (orange     '("#FF9837" "#dd8844" "brightred"     )) ;; Miozu13
   (green      '("#6DD672" "#99bb66" "green"         )) ;; Miozu11
   (teal       '("#44b9b1" "#44b9b1" "brightgreen"   )) ;; -/-
   (yellow     '("#E8D176" "#ECBE7B" "yellow"        )) ;; Miozu10
   (blue       '("#83D2FC" "#51afef" "brightblue"    )) ;; Miozu12
   (dark-blue  '("#2257A0" "#2257A0" "blue"          )) ;; -/-
   (magenta    '("#C974E6" "#c678dd" "brightmagenta" )) ;; Miozu08
   (violet     '("#FF9982" "#a9a1e1" "magenta"       )) ;; Miozu09
   (cyan       '("#40FFE2" "#46D9FF" "brightcyan"    )) ;; Miozu15
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"          )) ;; -/-

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-miozu-brighter-comments dark-cyan base4))
   (doc-comments   (doom-lighten (if doom-miozu-brighter-comments dark-cyan base4) 0.25))
   (constants      blue)
   (functions      magenta)
   (keywords       violet)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      base8)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-miozu-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-miozu-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-miozu-padded-modeline
      (if (integerp doom-miozu-padded-modeline) doom-miozu-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-miozu-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-miozu-brighter-modeline base8 highlight))

   ;; web-mode
   (web-mode-doctype-face           :foreground comments)
   (web-mode-html-tag-face          :foreground violet)
   (web-mode-html-tag-bracket-face  :foreground violet)
   (web-mode-html-entity-face       :foreground violet)
   (web-mode-html-attr-name-face    :foreground base5)
   (web-mode-block-control-face     :foreground blue)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground violet)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-miozu-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-miozu-theme.el ends here
