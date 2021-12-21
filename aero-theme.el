;;; aero-theme.el --- A practical theme -*- lexical-binding: t; -*-
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2019-2021 Jade Michael Thornton
;; URL: https://gitlab.com/thornjad/aero-theme
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.2.2
;;
;; This file is not part of GNU Emacs
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.

;;; Commentary:
;;
;; The Aero themes are a pair of light and dark themes which balance elegance
;; with practicality. They were originally part of [Aero
;; Emacs](https://gitlab.com/thornjad/aero) but were distilled for wider use.
;; The light theme draws color from the vibrancy of northern Minnesota in
;; Autumn, while the dark theme draws inspiration from my own [ClearNight
;; Retro](https://github.com/ClearNight/clearnight-retro-syntax) for
;; [Atom](https://atom.io).

;; Preview:
;;
;; ![Aero Theme](./images/aero-light-preview.png)
;;
;; ![Aero Dark Theme](./images/aero-dark-preview.png)

;; Installation:
;;
;; The Aero themes are not available on MELPA at this time, since they're still
;; undergoing some tweaking, and I'm not sure MELPA really needs more themes
;; right now.
;;
;; The easiest method of installation, if you have it set up already, is with
;; `straight.el' and `use-package':
;;
;;     (use-package aero-theme
;;       :straight (:host gitlab :repo "thornjad/aero-theme" :branch "main"))
;;
;; Alternatively, this repository may be cloned into your Emacs's
;; `custom-theme-directory'.
;;
;; Either way, activate the theme with
;;
;;     (load-theme 'aero)
;;     ;; or, for the dark variant:
;;     (load-theme 'aero-dark)

;;; Code:

(defgroup aero-theme nil
  "Supergroup for theme options"
  :group 'faces)

(defcustom aero-theme-font-height 100
  "Base font size."
  :group 'aero-theme)

(defcustom aero-theme-font
  (when (window-system)
    (cond
     ((x-list-fonts "IBM Plex Mono") "IBM Plex Mono")
		 ((x-list-fonts "Victor Mono") "Victor Mono")
		 ((x-list-fonts "Ubuntu Mono") "Ubuntu Mono")
		 (t "monospace")))
  "Base monospace font. Default depends on installed fonts."
  :group 'aero-theme)

(defcustom aero-theme-variable-pitch-font
  (when (window-system)
    (cond ((x-list-fonts "Ubuntu Light") "Ubuntu Light")
          ((x-list-fonts "Source Sans Pro") "Source Sans Pro")
          ((x-list-fonts "Fira Code Retina") "Fira Code Retina")
          ((x-list-fonts "Roboto") "Roboto")
          (t "Sans Serif")))
  "Base variable pitch font. Default depends on installed fonts."
  :group 'aero-theme)

(defmacro if-solaire (pos neg)
  "Return POS if Solaire is activated, otherwise NEG."
  `(if (or (and (boundp 'solaire-mode) solaire-mode)
           (and (boundp 'solaire-global-mode) solaire-global-mode))
       ,pos ,neg))

(deftheme aero)
(let ((class '((class color) (min-colors #xFF)))
      (aero-bg "#fefbf2")
      (aero-bg-alt "#f6f3e7")
      (aero-fg "#525C5A")

      (aero-space-base     "#655370")
      (aero-space-base-dim "#a094a2")
      (aero-space-base-dimmer "#D2CFD2")
      (aero-base0          "#fffdf9")
      (aero-base1          "#ece3cf")
      (aero-base2          "#e4e2e2")
      (aero-base3          "#96a7a9")
      (aero-base4          "#718388")
      ;; (aero-base5          "#626c6c")
      ;; (aero-base6          "#89867A")
      (aero-base7          "#424242")
      (aero-base8          "#d5c4a1")
      ;; (aero-base9          "#fafafa")
      (aero-base10 "#8B887D")
      (aero-base11 "#82aec5")
      (aero-base12 "#ECE9E0")
      (aero-base13 "#373426")
      (aero-act1 "#e7e5eb")
      ;; (aero-act2 "#d3d3e7")

      (aero-modeline-bg "#fefbf2")
      (aero-ttip "#373426")
      (aero-ttip-sel "#D5D2C8")
      (aero-ttip-bg "#ECE9E0")
      (aero-cursor-bg "#1B1809")
      (aero-comment "#829293")
      (aero-comment-bg "#EcEbEa")

      (aero-normal-black    "#4E4B3D")
      (aero-normal-white    "#ECF3F3")
      (aero-normal-red      "#5f081e")
      (aero-normal-orange   "#AB7146")
      (aero-normal-yellow   "#b58900")
      (aero-normal-green    "#968955")
      ;; (aero-normal-teal     "#35a69c")
      (aero-normal-blue     "#2aa198")
      (aero-normal-cyan     "#2aa198")
      (aero-normal-magenta  "#715ab1")

      ;; (aero-bright-black    "#928374")
      ;; (aero-bright-white    "#ffffd7")
      (aero-bright-red      "#dd8844")
      (aero-bright-orange   "#83684C")
      (aero-bright-yellow   "#d7af00")
      (aero-bright-green    "#507252")
      (aero-bright-blue     "#4B788D")
      ;; (aero-bright-cyan     "#204052")
      (aero-bright-magenta  "#6c71c4")

      (aero-faded-red "#aa2222")
      (aero-faded-red-dim "#eecccc")
      (aero-faded-orange "#432b00")
      (aero-faded-orange-dim "#edd5a8")
      (aero-faded-green "#22aa22")
      (aero-faded-green-dim "#cceecc")

      ;; (aero-intense-green  "#5f875f")
      (aero-brilliant-black     "#000000")
      (aero-brilliant-white     "#ffffff")
      ;; (aero-brilliant-green     "#005f00")
      ;; (aero-brilliant-yellow  "#d7d7af")
      ;; (aero-brilliant-blue    "#00005f")
      ;; (aero-brilliant-cyan    "#46d9ff")
      ;; (aero-brilliant-red       "#5f0000")
      )
  (custom-theme-set-faces
   'aero
   `(default ((t (:background ,(if-solaire aero-bg-alt aero-bg)
                  :foreground ,aero-fg
                  :font ,aero-theme-font
                  :height ,aero-theme-font-height))))
   `(variable-pitch
     ((t :font ,aero-theme-variable-pitch-font)))
   `(fringe ((t (:background ,(if-solaire aero-bg-alt aero-bg)))))
   `(cursor ((t (:background ,aero-cursor-bg))))
   `(mode-line
     ((t (:background ,aero-modeline-bg
          :foreground ,aero-space-base :height ,aero-theme-font-height
          :underline nil :overline ,aero-fg :box nil))))
   `(mode-line-active ((t (:inherit mode-line))))
   `(mode-line-inactive ((t (:inherit mode-line :foreground ,aero-space-base-dim))))
   `(header-line
     ((t (:inherit mode-line :height ,aero-theme-font-height
          :box (:line-width 2 :color ,aero-space-base-dimmer)
          :background ,aero-act1 :underline nil :overline nil))))
   `(tab-bar ((t (:inherit mode-line-inactive :overline nil :box (:line-width 3 :color ,(if-solaire aero-bg-alt aero-bg))))))
   `(tab-bar-tab ((t (:inherit mode-line :overline ,aero-space-base))))
   `(tab-bar-tab-inactive ((t (:inherit mode-line-inactive :slant italic))))
   `(hl-line ((t (:background ,aero-bg-alt))))
   `(region ((t (:background ,aero-base11 :foreground ,aero-bg))))
   `(highlight ((t (:background ,aero-base11 :foreground ,aero-bg))))
   `(secondary-selection ((t (:background ,aero-base0))))
   `(minibuffer-prompt ((t (:inherit default :foreground ,aero-normal-green :bold t))))
   `(vertical-border ((t (:foreground ,aero-base2 :background ,aero-bg))))
   `(window-divider ((t (:inherit vertical-border))))
   `(window-divider-first-pixel ((t (:inherit vertical-border :foreground ,aero-bg))))
   `(window-divider-last-pixel ((t (:inherit vertical-border :foreground ,aero-bg))))
   `(link ((t (:foreground ,aero-normal-blue :underline t))))
   `(link-visited ((t (:foreground ,aero-normal-magenta :underline t))))
   `(shadow ((t (:foreground ,aero-space-base-dim))))
   `(trailing-whitespace ((t (:background ,aero-bright-red))))
   `(escape-glyph ((t (:foreground ,aero-normal-blue))))
   `(lazy-highlight ((t (:background ,aero-normal-blue :foreground ,aero-base7 :distant-foreground ,aero-base8 :weight bold))))
   `(match ((t (:foreground ,aero-space-base :background ,aero-act1 :weight bold))))
   `(homoglyph ((t (:foreground ,aero-bright-yellow))))

   ;; Built-in syntax
   `(font-lock-builtin-face ((t (:foreground ,aero-bright-blue :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,aero-base4 :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,aero-comment :background ,aero-comment-bg :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,aero-normal-orange))))
   `(font-lock-keyword-face ((t (:foreground ,aero-normal-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,aero-base10 :slant italic))))
   `(font-lock-variable-name-face ((t (:foreground ,aero-bright-orange))))
   `(font-lock-type-face ((t (:foreground ,aero-bright-blue))))
   `(font-lock-warning-face ((t (:foreground ,aero-bright-yellow :bold t))))
   `(font-lock-doc-face ((t (:foreground ,aero-base3 :bold t))))

   ;; special
   `(error ((t (:foreground ,aero-bright-red :bold t))))
   `(success ((t (:foreground ,aero-normal-green :bold t))))
   `(warning ((t (:foreground ,aero-bright-yellow :bold t))))

   ;; Aero modeline
   `(aero/modeline-evil-normal ((t (:foreground ,aero-bg :background ,aero-normal-cyan))))
   `(aero/modeline-evil-insert ((t (:foreground ,aero-bg :background ,aero-normal-magenta))))
   `(aero/modeline-evil-visual ((t (:foreground ,aero-bg :background ,aero-normal-green))))
   `(aero/modeline-evil-replace ((t (:foreground ,aero-bg :background ,aero-bright-orange))))
   `(aero/modeline-evil-emacs ((t (:foreground ,aero-bg :background ,aero-bright-red))))
   `(aero/modeline-window-number ((t (:foreground ,aero-act1 :background ,aero-space-base))))
   `(aero/modeline-major-mode-active ((t (:foreground ,aero-space-base :bold t :background nil))))
   `(aero/modeline-major-mode-inactive ((t (:inherit aero/modeline-major-mode-active))))
   `(aero/modeline-modified ((t (:inherit error :height 180))))
   `(aero/modeline-not-modified ((t (:inherit success :height 180))))
   `(aero/modeline-read-only ((t (:inherit warning :height 180))))

   ;; Customize faces
   `(widget-field ((t (:background ,aero-base1))))
   `(custom-group-tag ((t (:foreground ,aero-normal-blue :weight bold))))
   `(custom-variable-tag ((t (:foreground ,aero-normal-blue :weight bold))))

   ;; whitespace-mode
   `(whitespace-space ((t (:foreground ,aero-base0))))
   `(whitespace-hspace ((t (:foreground ,aero-base0))))
   `(whitespace-tab ((t (:foreground ,aero-base0))))
   `(whitespace-newline ((t (:foreground ,aero-base0))))
   `(whitespace-trailing ((t (:foreground ,aero-bright-red))))
   `(whitespace-line ((t (:foreground ,aero-bright-red))))
   `(whitespace-space-before-tab ((t (:foreground ,aero-base0))))
   `(whitespace-indentation ((t (:foreground ,aero-base0))))
   `(whitespace-empty ((t (:foreground ,aero-base0))))
   `(whitespace-space-after-tab ((t (:foreground ,aero-base0))))

   ;; LSP
   `(lsp-ui-doc-background ((t (:background ,aero-act1 :box (:line-width 2 :color ,aero-space-base)))))
   `(lsp-ui-doc-header ((t (:foreground ,aero-space-base :background ,aero-act1 :underline ,aero-space-base))))
   `(lsp-headerline-breadcrumb-separator-face ((t (:height 0.8 :foreground ,aero-space-base))))
   `(lsp-ui-sideline-global ((t (:foreground ,aero-space-base :background ,aero-act1 :height 0.7))))
   `(lsp-ui-sideline-symbol ((t (:foreground ,aero-space-base :background ,aero-act1 :height 0.7))))
   `(lsp-ui-sideline-symbol-info ((t (:foreground ,aero-space-base :background ,aero-act1 :height 0.7 :slant italic))))
   `(lsp-ui-sideline-current-symbol ((t (:background ,aero-act1 :foreground ,aero-brilliant-black :height 0.7 :weight bold))))
   `(lsp-face-highlight-read ((t (:background nil :foreground nil :underline ,aero-base11))))

   ;; Displaying formfeed chars
   `(page-break-lines ((t (:foreground ,aero-base2))))
   `(formfeeder-line ((t (:strike-through ,aero-base2
                          :underline ,(when (not (window-system)) aero-base2)))))

   ;; Highlight indentation mode
   `(highlight-indentation-current-column-face ((t (:background ,aero-base1))))
   `(highlight-indentation-face ((t (:background ,aero-base0))))

   ;; indent-guide
   `(indent-guide-face ((t (:background ,aero-bg :foreground ,aero-base0 :slant normal))))

   ;; fill column indicator
   `(fill-column-indicator ((t (:foreground ,aero-base1))))

   ;; smartparens
   `(sp-pair-overlay-face ((t (:background ,aero-base1))))
   ;; Pair tags highlight
   `(sp-show-pair-match-face ((t (:underline t))))
   ;; Highlight for bracket without pair
   `(sp-show-pair-mismatch-face ((t (:background ,aero-bright-red :underline t))))

   ;; elscreen
   ;; Tab bar, not the tabs
   `(elscreen-tab-background-face ((t (:background ,aero-bg :box nil))))
   ;; The controls
   `(elscreen-tab-control-face
     (:background ,aero-base1 :foreground ,aero-bright-red :underline nil :box nil))
   ;; Current tab
   `(elscreen-tab-current-screen-face
     ((t (:background ,aero-base2 :foreground ,aero-normal-black :box nil))))
   ;; Inactive tab
   `(elscreen-tab-other-screen-face
     ((t (:background ,aero-base1 :foreground ,aero-base3 :underline nil :box nil))))

   ;; ag (The Silver Searcher)
   `(ag-hit-face ((t (:foreground ,aero-normal-blue))))
   `(ag-match-face ((t (:foreground ,aero-bright-red))))

   ;; diffs
   `(diff-changed ((t (:background ,aero-faded-orange-dim))))
   `(diff-added ((t (:background ,aero-faded-green-dim))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-removed ((t (:background ,aero-faded-red-dim))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-indicator-changed ((t (:inherit diff-changed))))
   `(diff-indicator-added ((t (:inherit diff-added))))
   `(diff-indicator-removed ((t (:inherit diff-removed))))

   ;; smerge
   `(smerge-upper ((t (:background nil))))
   `(smerge-lower ((t (:background nil))))
   `(smerge-refined-added ((t (:inherit diff-added))))
   `(smerge-refined-changed ((t (:inherit diff-changed))))
   `(smerge-refined-removed ((t (:inherit diff-removed))))
   `(smerge-base ((t (:background nil))))
   `(smerge-markers
     ((t (:inherit font-lock-comment-face :weight bold :background ,aero-base0))))

   ;; js2
   `(js2-warning ((t (:underline (:color ,aero-bright-yellow :style wave)))))
   `(js2-error ((t (:underline (:color ,aero-bright-red :style wave)))))
   `(js2-external-variable ((t (:underline (:color ,aero-normal-blue :style wave)))))
   `(js2-jsdoc-tag ((t (:background nil :foreground ,aero-base2))))
   `(js2-jsdoc-type ((t (:background nil :foreground ,aero-base3))))
   `(js2-jsdoc-value ((t (:background nil :foreground ,aero-base4))))
   `(js2-function-param ((t (:inherit font-lock-variable-name-face :slant italic))))
   `(js2-function-call ((t (:inherit font-lock-function-name-face))))
   `(js2-object-property ((t (:inherit font-lock-variable-name-face))))
   `(js2-instance-member ((t (:background nil :foreground ,aero-bright-yellow))))
   `(js2-private-member ((t (:background nil :foreground ,aero-bright-yellow))))
   `(js2-private-function-call ((t (:background nil :foreground ,aero-normal-blue))))
   `(js2-jsdoc-html-tag-name ((t (:background nil :foreground ,aero-base3))))
   `(js2-jsdoc-html-tag-delimiter ((t (:background nil :foreground ,aero-base4))))

   ;; rjsx
   `(rjsx-attr ((t (:inherit js2-object-property :slant italic))))

   ;; popup
   `(popup-face ((t (:underline nil :foreground ,aero-normal-yellow :background ,aero-base0))))
   `(popup-menu-mouse-face ((t (:underline nil :foreground ,aero-fg :background ,aero-normal-green))))
   `(popup-menu-selection-face ((t (:underline nil :foreground ,aero-fg :background ,aero-normal-green))))
   `(popup-tip-face ((t (:underline nil :foreground ,aero-base4 :background ,aero-base1))))

   ;; counsel
   `(counsel-M-x-key ((t (:foreground ,aero-bright-yellow))))
   `(counsel-action ((t (:foreground ,aero-normal-yellow :underline t))))
   `(counsel-bookmark-addressbook ((t (:foreground ,aero-bright-red))))
   `(counsel-bookmark-directory ((t (:foreground ,aero-normal-magenta))))
   `(counsel-bookmark-file ((t (:foreground ,aero-normal-blue))))
   `(counsel-bookmark-gnus ((t (:foreground ,aero-normal-magenta))))
   `(counsel-bookmark-info ((t (:foreground ,aero-normal-blue))))
   `(counsel-bookmark-man ((t (:foreground ,aero-normal-red))))
   `(counsel-bookmark-w3m ((t (:foreground ,aero-bright-yellow))))
   `(counsel-buffer-directory ((t (:foreground ,aero-brilliant-white :background ,aero-normal-blue))))
   `(counsel-buffer-not-saved ((t (:foreground ,aero-bright-red))))
   `(counsel-buffer-process ((t (:foreground ,aero-normal-yellow))))
   `(counsel-buffer-saved-out ((t (:foreground ,aero-bright-red))))
   `(counsel-buffer-size ((t (:foreground ,aero-normal-magenta))))
   `(counsel-candidate-number ((t (:foreground ,aero-normal-green))))
   `(counsel-ff-directory ((t (:foreground ,aero-normal-magenta))))
   `(counsel-ff-executable ((t (:foreground ,aero-normal-blue))))
   `(counsel-ff-file ((t (:foreground ,aero-normal-red))))
   `(counsel-ff-invalid-symlink ((t (:foreground ,aero-brilliant-white :background ,aero-bright-red))))
   `(counsel-ff-prefix ((t (:foreground ,aero-brilliant-black :background ,aero-bright-yellow))))
   `(counsel-ff-symlink ((t (:foreground ,aero-bright-yellow))))
   `(counsel-grep-cmd-line ((t (:foreground ,aero-normal-green))))
   `(counsel-grep-file ((t (:foreground ,aero-normal-magenta))))
   `(counsel-grep-finish ((t (:foreground ,aero-normal-blue))))
   `(counsel-grep-lineno ((t (:foreground ,aero-bright-yellow))))
   `(counsel-grep-match ((t (:inherit match))))
   `(counsel-grep-running ((t (:foreground ,aero-bright-red))))
   `(counsel-header ((t (:foreground ,aero-bright-magenta))))
   `(counsel-helper ((t (:foreground ,aero-normal-green))))
   `(counsel-history-deleted ((t (:foreground ,aero-brilliant-black :background ,aero-bright-red))))
   `(counsel-history-remote ((t (:foreground ,aero-bright-red))))
   `(counsel-lisp-completion-info ((t (:foreground ,aero-bright-yellow))))
   `(counsel-lisp-show-completion ((t (:foreground ,aero-bright-red))))
   `(counsel-locate-finish ((t (:foreground ,aero-brilliant-white :background ,aero-normal-green))))
   `(counsel-match ((t (:inherit match))))
   `(counsel-moccur-buffer ((t (:foreground ,aero-normal-blue :underline t))))
   `(counsel-prefarg ((t (:foreground ,aero-normal-blue))))
   `(counsel-selection ((t (:foreground ,aero-brilliant-white :background ,aero-base1))))
   `(counsel-selection-line ((t (:foreground ,aero-brilliant-white :background ,aero-base1))))
   `(counsel-separator ((t (:foreground ,aero-bright-red))))
   `(counsel-source-header ((t (:foreground ,aero-base4))))
   `(counsel-visible-mark ((t (:foreground ,aero-brilliant-black :background ,aero-base4))))

   ;; avy
   `(avy-lead-face
     ((t (:foreground ,aero-normal-white :background ,aero-normal-cyan :slant normal))))
   `(avy-lead-face-0
     ((t (:foreground ,aero-normal-white :background ,aero-bright-blue :slant normal))))
   `(avy-lead-face-1
     ((t (:foreground ,aero-normal-white :background ,aero-base0 :slant normal))))
   `(avy-lead-face-2
     ((t (:foreground ,aero-normal-white :background ,aero-normal-blue :slant normal))))

   ;; hi-lock-mode
   `(hi-black-b ((t (:foreground ,aero-brilliant-black :weight bold))))
   `(hi-black-hb ((t (:foreground ,aero-brilliant-black :weight bold :height 1.5))))
   `(hi-blue ((t (:foreground ,aero-normal-white :background ,aero-normal-blue))))
   `(hi-blue-b ((t (:foreground ,aero-normal-blue :weight bold))))
   `(hi-green ((t (:foreground ,aero-normal-white :background ,aero-normal-green))))
   `(hi-green-b ((t (:foreground ,aero-normal-green :weight bold))))
   `(hi-pink ((t (:foreground ,aero-normal-white :background ,aero-normal-magenta))))
   `(hi-red-b ((t (:foreground ,aero-bright-red :weight bold))))
   `(hi-yellow ((t (:foreground ,aero-normal-white :background ,aero-bright-yellow))))

   ;; company-mode
   `(company-scrollbar-bg ((t (:background ,aero-ttip-bg))))
   `(company-scrollbar-fg ((t (:background ,aero-ttip-sel))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation))))
   `(company-template-field ((t (:foreground ,aero-brilliant-black :background ,aero-bright-yellow))))
   `(company-echo-common ((,class (:background ,aero-base4 :foreground ,aero-bg))))
   `(company-preview ((,class (:background ,aero-ttip-bg :foreground ,aero-ttip))))
   `(company-preview-common ((,class (:background ,aero-ttip-bg :foreground ,aero-base4))))
   `(company-tooltip ((,class (:background ,aero-ttip-bg :foreground ,aero-ttip))))
   `(company-tooltip-annotation ((,class (:foreground ,aero-bright-blue))))
   `(company-tooltip-common ((,class (:background ,aero-ttip-bg :foreground ,aero-normal-magenta))))
   `(company-tooltip-common-selection ((,class (:foreground ,aero-normal-magenta))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-search ((,class (:inherit match))))
   `(company-tooltip-selection ((,class (:background ,aero-ttip-sel :foreground ,aero-base4))))

   ;; evil
   `(evil-ex-lazy-highlight ((,class (:background ,aero-normal-green :foreground ,aero-bg))))
   `(evil-ex-substitute-matches ((,class (:background ,aero-bright-red :foreground ,aero-bg))))
   `(evil-ex-substitute-replacement ((,class (:background ,aero-bright-green :foreground ,aero-bg))))

   ;; tool tips
   `(tooltip ((t (:foreground ,aero-normal-yellow :background ,aero-base0))))

   ;; term
   `(term-color-black ((t (:foreground ,aero-base1 :background ,aero-base0))))
   `(term-color-blue ((t (:foreground ,aero-normal-blue :background ,aero-normal-blue))))
   `(term-color-cyan ((t (:foreground ,aero-normal-blue :background ,aero-normal-blue))))
   `(term-color-green ((t (:foreground ,aero-normal-green :background ,aero-normal-green))))
   `(term-color-magenta ((t (:foreground ,aero-normal-magenta :background ,aero-normal-magenta))))
   `(term-color-red ((t (:foreground ,aero-bright-red :background ,aero-bright-red))))
   `(term-color-white ((t (:foreground ,aero-normal-yellow :background ,aero-normal-yellow))))
   `(term-color-yellow ((t (:foreground ,aero-bright-yellow :background ,aero-bright-yellow))))
   `(term-default-fg-color ((t (:foreground ,aero-fg))))
   `(term-default-bg-color ((t (:background ,aero-bg))))

   ;; message-mode
   `(message-header-to ((t (:inherit font-lock-variable-name-face))))
   `(message-header-cc ((t (:inherit font-lock-variable-name-face))))
   `(message-header-subject ((t (:foreground ,aero-bright-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,aero-bright-yellow :weight bold))))
   `(message-header-other ((t (:inherit font-lock-variable-name-face))))
   `(message-header-name ((t (:inherit font-lock-keyword-face))))
   `(message-header-xheader ((t (:foreground ,aero-normal-blue))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-mml ((t (:foreground ,aero-normal-green :weight bold))))

   ;; org-mode
   `(org-agenda-date-today ((t (:foreground ,aero-fg :weight bold :slant italic))))
   `(org-agenda-done ((t (:foreground ,aero-normal-blue))))
   `(org-agenda-structure ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,aero-fg :weight bold))))
   `(org-block ((t (:inherit fixed-pitch :background ,aero-base0))))
   `(org-block-begin-line ((t (:background ,aero-base0))))
   `(org-block-end-line ((t (:background ,aero-base0))))
   `(org-code ((t (:inherit fixed-pitch :background ,aero-base0))))
   `(org-column ((t (:background ,aero-normal-black))))
   `(org-column-title ((t (:background ,aero-normal-black :underline t :weight bold))))
   `(org-date ((t (:foreground ,aero-normal-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,aero-bright-red))))
   `(org-document-info ((t (:foreground ,aero-normal-blue))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-document-title ((t (:height 1.4 :underline nil :weight bold))))
   `(org-done ((t (:foreground ,aero-normal-blue :weight bold :bold t))))
   `(org-drawer ((t (:inherit font-lock-function-name-face))))
   `(org-ellipsis ((t (:foreground ,aero-base3))))
   `(org-footnote ((t (:foreground ,aero-normal-blue :underline t))))
   `(org-formula ((t (:foreground ,aero-bright-yellow))))
   `(org-headline-done ((t (:foreground ,aero-normal-blue))))
   `(org-hide ((t (:foreground ,aero-bg))))
   `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   `(org-latex-and-related ((t (:foreground ,aero-normal-blue))))
   `(org-level-1 ((t (:height 1.13 :weight bold))))
   `(org-level-2 ((t (:height 1.09 :weight bold))))
   `(org-level-3 ((t (:height 1.05 :weight bold))))
   `(org-level-4 ((t (:weight bold))))
   `(org-level-5 ((t (:weight bold))))
   `(org-level-6 ((t (:weight bold))))
   `(org-level-7 ((t (:weight bold))))
   `(org-level-8 ((t (:weight bold))))
   `(org-link ((t (:foreground ,aero-normal-blue :underline t))))
   `(org-list-dt ((t (:bold t :weight bold))))
   `(org-meta-line ((t (:inherit (shadow font-lock-comment-face fixed-pitch)))))
   `(org-property-value ((t (:inherit fixed-pitch))))
   `(org-scheduled ((t (:foreground ,aero-bright-yellow))))
   `(org-scheduled-previously ((t (:foreground ,aero-bright-red))))
   `(org-scheduled-today ((t (:foreground ,aero-normal-blue))))
   `(org-sexp-date ((t (:foreground ,aero-normal-blue :underline t))))
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-table ((t (:inherit fixed-pitch :foreground ,aero-normal-blue))))
   `(org-tag ((t (:inherit (shadow fixed-pitch) :bold t :weight bold :height 0.8))))
   `(org-time-grid ((t (:foreground ,aero-bright-yellow))))
   `(org-todo ((t (:foreground ,aero-bright-red :weight bold :bold t))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   `(org-warning ((t (:foreground ,aero-bright-red :weight bold :underline nil :bold t))))

   ;; org-habit
   `(org-habit-clear-face ((t (:background ,aero-normal-blue))))
   `(org-habit-clear-future-face ((t (:background ,aero-normal-blue))))
   `(org-habit-ready-face ((t (:background ,aero-normal-green))))
   `(org-habit-ready-future-face ((t (:background ,aero-normal-green))))
   `(org-habit-alert-face ((t (:background ,aero-bright-yellow))))
   `(org-habit-alert-future-face ((t (:background ,aero-bright-yellow))))
   `(org-habit-overdue-face ((t (:background ,aero-bright-red))))
   `(org-habit-overdue-future-face ((t (:background ,aero-bright-red))))

   ;; jupyter via ein
   `(ein:cell-input-area ((t (:background ,aero-bg))))
   `(ein:cell-input-prompt ((t (:foreground ,aero-normal-blue :slant italic :box ,aero-base1))))
   `(ein:cell-output-prompt ((t (:foreground ,aero-normal-cyan :slant italic :box ,aero-base1))))

   ;; elfeed
   `(elfeed-search-title-face ((t (:foreground ,aero-base2  ))))
   `(elfeed-search-unread-title-face ((t (:foreground ,aero-fg))))
   `(elfeed-search-date-face ((t (:inherit font-lock-builtin-face :underline t))))
   `(elfeed-search-feed-face ((t (:inherit font-lock-variable-name-face))))
   `(elfeed-search-tag-face ((t (:inherit font-lock-keyword-face))))
   `(elfeed-search-last-update-face ((t (:inherit font-lock-comment-face))))
   `(elfeed-search-unread-count-face ((t (:inherit font-lock-comment-face))))
   `(elfeed-search-filter-face ((t (:inherit font-lock-string-face))))

   ;; markdown-mode
   `(markdown-header-face-1 ((t (:foreground ,aero-normal-blue))))
   `(markdown-header-face-2 ((t (:foreground ,aero-bright-yellow))))
   `(markdown-header-face-3 ((t (:foreground ,aero-normal-magenta))))
   `(markdown-header-face-4 ((t (:foreground ,aero-bright-red))))
   `(markdown-header-face-5 ((t (:foreground ,aero-normal-green))))
   `(markdown-header-face-6 ((t (:foreground ,aero-normal-blue))))
   `(markdown-pre-face ((t (:inherit fixed-pitch))))
   `(markdown-inline-code-face ((t (:inherit fixed-pitch))))

   ;; ace-jump-mode
   `(ace-jump-face-background ((t (:foreground ,aero-base3 :background ,aero-bg :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,aero-bright-red :background ,aero-bg :inverse-video nil))))

   ;; ace-window
   `(aw-background-face ((t (:forground  ,aero-normal-yellow :background ,aero-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:foreground ,aero-bright-red :background ,aero-bg :height 4.0))))

   ;; auto-complete
   `(ac-completion-face ((,class (:background ,aero-ttip-bg :foreground ,aero-ttip))))

   ;; show-paren
   `(show-paren-match ((t (:foreground ,aero-base13 :underline t))))
   `(show-paren-mismatch ((t (:background ,aero-bright-red :foreground ,aero-base1 :weight bold))))

   ;; swiper
   `(swiper-match-face-1 ((t (:inherit match))))
   `(swiper-match-face-2 ((t (:inherit swiper-match-face-1))))
   `(swiper-match-face-3 ((t (:inherit swiper-match-face-1))))
   `(swiper-match-face-4 ((t (:inherit swiper-match-face-1))))

   ;; ivy
   `(ivy-current-match ((t (:inherit highlight :foreground ,aero-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,aero-fg))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,aero-fg :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,aero-fg :weight bold))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,aero-fg :weight bold))))

   ;; ivy-posframe
   `(ivy-posframe ((t (:background ,aero-modeline-bg))))
   `(ivy-posframe-border ((t (:box (:color ,aero-space-base :width 2) :background ,aero-space-base))))

   ;; magit
   `(magit-bisect-bad ((t (:foreground ,aero-bright-red))))
   `(magit-bisect-good ((t (:foreground ,aero-normal-green))))
   `(magit-bisect-skip ((t (:foreground ,aero-bright-yellow))))
   `(magit-blame-heading ((t (:foreground ,aero-fg :background ,aero-base1))))
   `(magit-branch-local ((t (:foreground ,aero-normal-blue))))
   `(magit-branch-current ((t (:underline ,aero-normal-blue :inherit magit-branch-local))))
   `(magit-branch-remote ((t (:foreground ,aero-normal-magenta))))
   `(magit-cherry-equivalent ((t (:foreground ,aero-normal-magenta))))
   `(magit-cherry-unmatched ((t (:foreground ,aero-normal-blue))))
   `(magit-diff-added ((t (:foreground ,aero-faded-green))))
   `(magit-diff-added-highlight ((t (:foreground ,aero-faded-green :background ,aero-faded-green-dim))))
   `(magit-diff-base
     ((t (:background ,aero-space-base-dimmer :foreground ,aero-space-base))))
   `(magit-diff-base-highlight
     ((t (:weight bold))))
   `(magit-diff-context ((t (:foreground "grey50" :background "grey95" :extend t))))
   `(magit-diff-context-highlight ((t (:inherit magit-diff-context :background ,aero-base12 :extend t))))
   `(magit-diff-file-heading ((t (:weight normal))))
   `(magit-diff-file-heading-highlight ((t (:weight bold))))
   `(magit-diff-file-heading-selection ((t (:inherit magit-diff-file-heading-highlight :foreground "salmon4"))))
   `(magit-diff-hunk-heading
     ((t (:background ,aero-space-base-dimmer :foreground ,aero-space-base))))
   `(magit-diff-hunk-heading-highlight
     ((t (:weight bold))))
   `(magit-diff-hunk-heading-selection
     ((t (:inherit magit-diff-hunk-heading-highlight :foreground "salmon4"))))
   `(magit-diff-lines-heading
     ((t (:inherit magit-diff-hunk-heading-highlight :background "LightSalmon3"))))
   `(magit-diff-removed ((t (:foreground ,aero-faded-red))))
   `(magit-diff-removed-highlight
     ((t (:foreground ,aero-faded-red :background ,aero-faded-red-dim))))
   `(magit-diffstat-added ((t (:foreground ,aero-bright-green))))
   `(magit-diffstat-removed ((t (:foreground ,aero-bright-red))))
   `(magit-dimmed ((t (:foreground ,aero-base2))))
   `(magit-hash ((t (:foreground ,aero-normal-green))))
   `(magit-log-author ((t (:foreground ,aero-bright-red))))
   `(magit-log-date ((t (:foreground ,aero-normal-blue))))
   `(magit-log-graph ((t (:foreground ,aero-base2))))
   `(magit-process-ng ((t (:foreground ,aero-bright-red :weight bold))))
   `(magit-process-ok ((t (:foreground ,aero-normal-green :weight bold))))
   `(magit-reflog-amend ((t (:foreground ,aero-normal-magenta))))
   `(magit-reflog-checkout ((t (:foreground ,aero-normal-blue))))
   `(magit-reflog-cherry-pick ((t (:foreground ,aero-normal-green))))
   `(magit-reflog-commit ((t (:foreground ,aero-normal-green))))
   `(magit-reflog-merge ((t (:foreground ,aero-normal-green))))
   `(magit-reflog-other ((t (:foreground ,aero-normal-blue))))
   `(magit-reflog-rebase ((t (:foreground ,aero-normal-magenta))))
   `(magit-reflog-remote ((t (:foreground ,aero-normal-blue))))
   `(magit-reflog-reset ((t (:foreground ,aero-bright-red))))
   `(magit-refname ((t (:foreground ,aero-base3))))
   `(magit-section-heading ((t (:foreground ,aero-normal-green :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,aero-bright-yellow :weight bold))))
   `(magit-section-highlight ((t (:inherit magit-diff-file-heading-highlight :weight bold))))
   `(magit-sequence-drop ((t (:foreground ,aero-bright-yellow))))
   `(magit-sequence-head ((t (:foreground ,aero-normal-blue))))
   `(magit-sequence-part ((t (:foreground ,aero-bright-yellow))))
   `(magit-sequence-stop ((t (:foreground ,aero-normal-green))))
   `(magit-signature-bad ((t (:foreground ,aero-bright-red :weight bold))))
   `(magit-signature-error ((t (:foreground ,aero-bright-red))))
   `(magit-signature-expired ((t (:foreground ,aero-bright-yellow))))
   `(magit-signature-good ((t (:foreground ,aero-normal-green))))
   `(magit-signature-revoked ((t (:foreground ,aero-normal-magenta))))
   `(magit-signature-untrusted ((t (:foreground ,aero-normal-blue))))
   `(magit-tag ((t (:foreground ,aero-bright-yellow))))

   ;; git-gutter and git-gutter-fringe
   `(git-gutter:added ((,class (:inherit diff-added))))
   `(git-gutter:deleted ((,class (:inherit diff-removed))))
   `(git-gutter:modified ((,class (:inherit diff-changed))))
   `(git-gutter+-added ((,class (:inherit diff-added))))
   `(git-gutter+-deleted ((,class (:inherit diff-removed))))
   `(git-gutter+-modified ((,class (:inherit diff-changed))))
   `(git-gutter-fr:added ((,class (:inherit diff-added :foreground ,aero-faded-green))))
   `(git-gutter-fr:deleted ((,class (:inherit diff-removed :foreground ,aero-faded-red))))
   `(git-gutter-fr:modified ((,class (:inherit diff-changed :foreground ,aero-faded-orange))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:color ,aero-base3 :style line)))))
   `(flyspell-incorrect ((t (:underline (:color ,aero-bright-red :style line)))))

   ;; eprime-mode
   `(eprime-banned-words-face ((t (:inherit default :underline (:color ,aero-bright-blue :style wave)))))

   ;; flycheck
   `(flycheck-warning ((t (:underline (:color ,aero-bright-yellow :style line)))))
   `(flycheck-error ((t (:underline (:color ,aero-bright-red :style line)))))
   `(flycheck-info ((t (:underline (:color ,aero-bright-green :style line)))))
   `(flycheck-fringe-error ((,class (:inherit error :weight bold))))
   `(flycheck-fringe-info ((,class (:inherit font-lock-keyword-face :weight bold))))
   `(flycheck-fringe-warning ((,class (:inherit warning :weight bold))))

   ;; langtool
   `(langtool-errline ((t (:foreground ,aero-normal-black :background ,aero-bright-red))))
   `(langtool-correction-face ((t (:foreground ,aero-bright-yellow :weight bold))))

   ;; latex
   `(font-latex-bold-face ((t (:foreground ,aero-normal-green :bold t))))
   `(font-latex-italic-face ((t (:foreground ,aero-normal-green :underline t))))
   `(font-latex-math-face ((t (:foreground ,aero-base4))))
   `(font-latex-script-char-face ((t (:foreground ,aero-normal-blue))))
   `(font-latex-sectioning-5-face ((t (:foreground ,aero-bright-yellow :bold t))))
   `(font-latex-sedate-face ((t (:foreground ,aero-base3))))
   `(font-latex-string-face ((t (:foreground ,aero-bright-yellow))))
   `(font-latex-verbatim-face ((t (:foreground ,aero-base3))))
   `(font-latex-warning-face ((t (:foreground ,aero-bright-red :weight bold))))
   `(preview-face ((t (:background ,aero-base0))))

   ;; mu4e
   `(mu4e-header-key-face ((t (:foreground ,aero-normal-green :weight bold ))))
   `(mu4e-unread-face ((t (:foreground ,aero-normal-blue :weight bold ))))
   `(mu4e-highlight-face ((t (:foreground ,aero-normal-green))))

   ;; shell script
   `(sh-quoted-exec ((t (:foreground ,aero-normal-magenta))))
   `(sh-heredoc ((t (:foreground ,aero-bright-yellow))))

   ;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,aero-fg))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,aero-bright-red))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,aero-base2))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,aero-bright-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,aero-normal-blue))))

   ;; treemacs
   `(treemacs-fringe-indicator-face ((t (:foreground ,aero-space-base-dim))))

   ;; widget faces
   `(widget-button-pressed-face ((t (:foreground ,aero-bright-red))))
   `(widget-documentation-face ((t (:foreground ,aero-normal-green))))
   `(widget-field ((t (:foreground ,aero-fg :background ,aero-base1))))
   `(widget-single-line-field ((t (:foreground ,aero-fg :background ,aero-base1))))

   ;; Solaire
   `(solaire-mode-line-face ((t (:inherit mode-line))))
   `(solaire-mode-line-inactive-face ((t (:inherit mode-line-inactive))))
   `(solaire-default-face ((t (:inherit default :background ,aero-bg))))
   `(solaire-fringe-face ((t (:inherit fringe :background ,aero-bg))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,aero-normal-blue))))
   `(eshell-ls-archive ((t (:foreground ,aero-base4))))
   `(eshell-ls-backup ((t (:foreground ,aero-base3))))
   `(eshell-ls-clutter ((t (:foreground ,aero-bright-yellow :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,aero-normal-yellow))))
   `(eshell-ls-executable ((t (:weight bold))))
   `(eshell-ls-missing ((t (:foreground ,aero-bright-red :bold t))))
   `(eshell-ls-product ((t (:foreground ,aero-bright-red))))
   `(eshell-ls-readonly ((t (:foreground ,aero-base4))))
   `(eshell-ls-special ((t (:foreground ,aero-bright-yellow :bold t))))
   `(eshell-ls-symlink ((t (:foreground ,aero-bright-red))))
   `(eshell-ls-unreadable ((t (:foreground ,aero-bright-red :bold t))))

   ;; eshell-prompt-extras
   `(epe-git-dir-face ((t (:inherit eshell-ls-directory :bold t))))

   ;; wgrep
   `(wgrep-delete-face ((t (:strike-through ,aero-bright-red))))
   `(wgrep-done-face ((t (:foreground ,aero-normal-blue))))
   `(wgrep-face ((t (:underline (:color ,aero-bright-yellow :style line)))))
   `(wgrep-file-face ((t (:inherit highlight))))
   `(wgrep-reject-face ((t (:foreground ,aero-bright-red :bold t))))

   ;; hydra
   `(hydra-face-red ((t (:foreground ,aero-bright-red :weight bold))))
   `(hydra-face-blue ((t (:foreground ,aero-normal-blue :weight bold))))
   `(hydra-face-amaranth ((t (:foreground ,aero-bright-yellow :weight bold))))
   `(hydra-face-pink ((t (:foreground ,aero-normal-magenta :weight bold))))
   `(hydra-face-teal ((t (:foreground ,aero-normal-blue :weight bold))))

   ;; which-function-mode
   `(which-func ((t (:foreground ,aero-normal-blue))))

   ;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face ((t (:background ,aero-normal-black)))))

  (setq window-divider-default-right-width 1)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode)

  ;; Ensure org-mode uses variable pitch
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'aero)
(provide 'aero-theme)
;;; aero-theme.el ends here
