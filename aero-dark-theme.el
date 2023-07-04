;;; aero-dark-theme.el --- A practical theme -*- lexical-binding: t -*-
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2019-2023 Jade Michael Thornton
;; URL: https://gitlab.com/thornjad/aero-theme
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.3.0
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
;;
;; This is the dark companion to the light `aero-theme'. See ./aero-theme.el and
;; ./readme.md for more information.

;;; Code:

(defgroup aero-dark-theme nil
  "Supergroup for theme options"
  :group 'faces)

(defcustom aero-theme-font-height 100
  "Base font size."
  :group 'aero-theme)

(defcustom aero-theme-font
  (if (window-system)
      (cond
       ((x-list-fonts "JetBrains Mono") "JetBrains Mono")
       ((x-list-fonts "IBM Plex Mono") "IBM Plex Mono")
	     ((x-list-fonts "Victor Mono") "Victor Mono")
	     ((x-list-fonts "Ubuntu Mono") "Ubuntu Mono")
	     (t "monospace"))
    "monospace")
  "Base monospace font. Default depends on installed fonts."
  :group 'aero-theme)

(defcustom aero-theme-variable-pitch-font
  (if (window-system)
    (cond ((x-list-fonts "Ubuntu Light") "Ubuntu Light")
          ((x-list-fonts "Source Sans Pro") "Source Sans Pro")
          ((x-list-fonts "Fira Code Retina") "Fira Code Retina")
          ((x-list-fonts "Roboto") "Roboto")
          (t "Sans Serif"))
    "Sans Serif")
  "Base variable pitch font. Default depends on installed fonts."
  :group 'aero-theme)

(defmacro if-solaire (pos neg)
  "Return POS if Solaire is activated, otherwise NEG."
  `(if (or (and (boundp 'solaire-mode) solaire-mode)
           (and (boundp 'solaire-global-mode) solaire-global-mode))
       ,pos ,neg))

(deftheme aero-dark)
(let ((class '((class color) (min-colors #xFF)))
      (aero-bg "#26282b")
      (aero-bg-alt "#232526")
      (aero-fg "#d5dae3")

      (aero-grey0 "#373426")
      (aero-grey05 "#474436")
      (aero-grey1 "#4e4b3d")
      (aero-grey2 "#6b6b6b")
      (aero-grey3 "#979797")
      (aero-grey4 "#9295a0")
      (aero-grey5 "#b4b9c1")
      (aero-grey6 "#444155")
      (aero-space-base "#b2b2b2")
      (aero-space-base-dim "#686868")
      (aero-bg-magenta "#5d4d7a")
      ;; (aero-space-magenta "#bc6ec5")

      (aero-normal-black "#282828")
      (aero-normal-white "#e7e5eb")
      (aero-normal-red "#d7875f")
      (aero-normal-orange "#c38152")
      (aero-normal-yellow "#C8A827")
      (aero-normal-green "#819a2c")
      (aero-normal-blue "#4f97d7")
      ;; (aero-normal-teal "#8ebcbb")
      (aero-normal-cyan "#8ec07c")
      (aero-normal-magenta "#bbb8d8")
      (aero-normal-mint "#a3be8c")

      ;; (aero-bright-black "#928374")
      ;; (aero-bright-white "#ffffd7")
      (aero-bright-red "#dd8844")
      (aero-bright-orange "#dd8117")
      (aero-bright-yellow "#fabd2f")
      (aero-bright-green "#9db14d")
      (aero-bright-blue "#83a598")
      (aero-bright-cyan "#8ec07c")
      (aero-bright-magenta "#b76cc4")

      ;; (aero-intense-green "#5f875f")
      ;; (aero-intense-burgundy "#5f081e")
      (aero-brilliant-black "#000000")
      (aero-brilliant-white "#ffffff")
      (aero-brilliant-green "#005f00")
      ;; (aero-brilliant-yellow "#d7d7af")
      ;; (aero-brilliant-blue "#00005f")
      ;; (aero-brilliant-cyan "#46d9ff")
      (aero-brilliant-red "#5f0000"))
  (custom-theme-set-faces
   'aero-dark
   `(default ((t (:background ,(if-solaire aero-bg-alt aero-bg)
                  :foreground ,aero-fg
                  :font ,aero-theme-font
                  :height ,aero-theme-font-height))))
   `(variable-pitch
     ((t :font ,aero-theme-variable-pitch-font)))
   `(fringe ((t (:background ,(if-solaire aero-bg-alt aero-bg)))))
   `(cursor ((t (:background ,aero-fg))))
   `(mode-line ((t (:background ,aero-bg-alt :foreground ,aero-space-base))))
   `(mode-line-active ((t (:inherit mode-line))))
   `(mode-line-inactive ((t (:inherit mode-line :foreground ,aero-space-base-dim))))
   `(header-line ((t (:inherit mode-line :overline nil :underline nil
                      :box (:line-width 2 :color ,aero-bg-magenta) :height ,aero-theme-font-height))))
   `(tab-bar ((t (:inherit mode-line-inactive))))
   `(tab-bar-tab ((t (:inherit mode-line :overline ,aero-space-base))))
   `(tab-bar-tab-inactive ((t (:inherit mode-line-inactive :slant italic))))
   `(hl-line ((t (:background ,aero-grey0))))
   `(region ((t (:background ,aero-grey6 :foreground ,aero-grey5))))
   `(secondary-selection ((t (:background ,aero-grey0))))
   `(minibuffer-prompt ((t (:inherit default :foreground ,aero-normal-green :bold t))))
   `(vertical-border ((t (:foreground ,aero-grey1 :background ,aero-grey1))))
   `(window-divider ((t (:foreground ,aero-grey1))))
   `(link ((t (:foreground ,aero-normal-blue :underline t))))
   `(link-visited ((t (:foreground ,aero-normal-magenta :underline t))))
   `(shadow ((t (:foreground ,aero-grey2))))
   `(trailing-whitespace ((t (:background ,aero-bright-red))))
   `(escape-glyph ((t (:foreground ,aero-normal-blue))))
   `(highlight ((t (:background ,aero-grey2 :foreground ,aero-fg))))
   `(homoglyph ((t (:foreground ,aero-bright-yellow))))
   `(match ((t (:foreground ,aero-normal-black :background ,aero-normal-blue))))

   ;; Built-in syntax
   `(font-lock-builtin-face ((t (:foreground ,aero-grey5 :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,aero-normal-magenta))))
   `(font-lock-comment-face ((,class (:foreground ,aero-grey4 :background ,aero-bg :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,aero-bright-orange))))
   `(font-lock-keyword-face ((t (:foreground ,aero-normal-yellow))))
   `(font-lock-string-face ((t (:foreground ,aero-bright-green :slant italic))))
   `(font-lock-variable-name-face ((t (:foreground ,aero-normal-orange))))
   `(font-lock-type-face ((t (:foreground ,aero-normal-blue :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,aero-bright-yellow :bold t))))
   `(font-lock-doc-face ((t (:foreground ,aero-grey3 :bold t))))

   ;; special
   `(error ((t (:foreground ,aero-bright-red :weight bold))))
   `(success ((t (:foreground ,aero-normal-green :weight bold))))
   `(warning ((t (:foreground ,aero-bright-yellow :weight bold))))

   ;; Aero modeline
   `(aero/modeline-evil-normal ((t (:foreground ,aero-grey4))))
   `(aero/modeline-evil-insert ((t (:foreground ,aero-normal-blue))))
   `(aero/modeline-evil-visual ((t (:foreground ,aero-normal-green))))
   `(aero/modeline-evil-replace ((t (:foreground ,aero-bright-orange))))
   `(aero/modeline-evil-operator ((t (:foreground ,aero-bright-yellow))))
   `(aero/modeline-evil-motion ((t (:foreground ,aero-grey3))))
   `(aero/modeline-evil-emacs ((t (:foreground ,aero-bright-red))))
   `(aero/modeline-major-mode ((t (:inherit mode-line-buffer-id))))
   `(aero/modeline-modified ((t (:inherit error :height 180))))
   `(aero/modeline-not-modified ((t (:inherit success :height 180))))
   `(aero/modeline-read-only ((t (:inherit warning :height 180))))
   `(aero/modeline-bar ((t (:background ,aero-bg-magenta))))
   `(aero/modeline-bar-inactive ((t (:background ,aero-space-base-dim))))

   ;; centaur tabs
   `(centaur-tabs-default ((t (:background ,aero-grey0 :foreground ,aero-grey4))))
   `(centaur-tabs-selected ((t (:inherit mode-line :background ,aero-bg))))
   `(centaur-tabs-unselected ((t (:inherit mode-line-inactive))))
   `(centaur-tabs-selected-modified ((t (:inherit centaur-tabs-selected))))
   `(centaur-tabs-unselected-modified ((t (:inherit centaur-tabs-unselected))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit centaur-tabs-selected :foreground ,aero-bright-red))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit centaur-tabs-unselected :foreground ,aero-bright-red))))

   ;; Customize faces
   `(widget-field ((t (:background ,aero-grey1))))
   `(custom-group-tag ((t (:foreground ,aero-normal-blue :weight bold))))
   `(custom-variable-tag ((t (:foreground ,aero-normal-blue :weight bold))))

   ;; whitespace-mode
   `(whitespace-space ((t (:foreground ,aero-grey0))))
   `(whitespace-hspace ((t (:foreground ,aero-grey0))))
   `(whitespace-tab ((t (:foreground ,aero-grey0))))
   `(whitespace-newline ((t (:foreground ,aero-grey0))))
   `(whitespace-trailing ((t (:foreground ,aero-bright-red))))
   `(whitespace-line ((t (:foreground ,aero-bright-red))))
   `(whitespace-space-before-tab ((t (:foreground ,aero-grey0))))
   `(whitespace-indentation ((t (:foreground ,aero-grey0))))
   `(whitespace-empty ((t (:foreground ,aero-grey0))))
   `(whitespace-space-after-tab ((t (:foreground ,aero-grey0))))

   ;; LSP
   `(lsp-ui-doc-background ((t (:background ,aero-bg-alt))))
   `(lsp-ui-doc-header ((t (:foreground ,aero-space-base :background ,aero-bg-alt :underline ,aero-space-base))))
   `(lsp-ui-sideline-code-action ((t (:foreground ,aero-grey6 :underline t))))
   `(lsp-headerline-breadcrumb-separator-face ((t (:height 0.7 :foreground ,aero-grey6))))
   `(lsp-ui-sideline-global ((t (:foreground ,aero-grey6 :background ,aero-bg-alt :height 0.7))))
   `(lsp-ui-sideline-symbol ((t (:foreground ,aero-grey6 :background ,aero-bg-alt :height 0.7))))
   `(lsp-ui-sideline-symbol-info ((t (:foreground ,aero-grey6 :background ,aero-bg-alt :height 0.7 :slant italic))))
   `(lsp-ui-sideline-current-symbol ((t (:background ,aero-grey6 :foreground ,aero-brilliant-black :height 0.7 :weight bold))))
   `(lsp-face-highlight-read ((t (:background ,aero-grey6 :foreground unspecified))))

   ;; Displaying formfeed chars
   `(page-break-lines ((t (:foreground ,aero-grey2))))
   `(formfeeder-line ((t (:strike-through ,aero-grey2 :underline ,(when (not (window-system)) aero-grey2)))))

   ;; Highlight indentation mode
   `(highlight-indentation-current-column-face ((t (:background ,aero-grey1))))
   `(highlight-indentation-face ((t (:background ,aero-grey0))))

   ;; indent-guide
   `(indent-guide-face ((t (:foreground ,aero-grey0 :slant normal))))

   ;; fill column indicator
   `(fill-column-indicator ((t (:foreground ,aero-grey05))))

   ;; pulse
   `(pulse-highlight-start-face ((t (:background ,aero-bg-magenta))))

   ;; smartparens
   `(sp-pair-overlay-face ((t (:background ,aero-grey1))))
   ;; Pair tags highlight
   `(sp-show-pair-match-face ((t (:inherit show-paren-match))))
   ;; Highlight for bracket without pair
   `(sp-show-pair-mismatch-face ((t (:inherit show-paren-mismatch))))

   ;; elscreen
   ;; Tab bar, not the tabs
   `(elscreen-tab-background-face ((t (:background ,aero-bg :box nil))))
   ;; The controls
   `(elscreen-tab-control-face
     (:background ,aero-grey1 :foreground ,aero-bright-red :underline nil :box nil))
   ;; Current tab
   `(elscreen-tab-current-screen-face
     ((t (:background ,aero-grey2 :foreground ,aero-normal-black :box nil))))
   ;; Inactive tab
   `(elscreen-tab-other-screen-face
     ((t (:background ,aero-grey1 :foreground ,aero-grey3 :underline nil :box nil))))

   ;; ag (The Silver Searcher)
   `(ag-hit-face ((t (:foreground ,aero-normal-blue))))
   `(ag-match-face ((t (:foreground ,aero-bright-red))))

   ;; diffs
   `(diff-changed ((t (:background ,aero-bg :foreground ,aero-normal-yellow))))
   `(diff-added ((t (:background ,aero-bg :foreground ,aero-normal-green))))
   `(diff-refine-added ((t (:background ,aero-brilliant-green :foreground ,aero-bright-green))))
   `(diff-removed ((t (:background ,aero-bg :foreground ,aero-bright-red))))
   `(diff-refine-removed ((t (:background ,aero-brilliant-red :foreground ,aero-bright-red))))
   `(diff-indicator-changed ((t (:inherit diff-changed))))
   `(diff-indicator-added ((t (:inherit diff-added))))
   `(diff-indicator-removed ((t (:inherit diff-removed))))

   ;; smerge
   `(smerge-upper ((t (:background unspecified))))
   `(smerge-lower ((t (:background unspecified))))
   `(smerge-refined-added ((t (:inherit diff-added))))
   `(smerge-refined-changed ((t (:inherit diff-changed))))
   `(smerge-refined-removed ((t (:inherit diff-removed))))
   `(smerge-base ((t (:background unspecified))))
   `(smerge-markers
     ((t (:inherit font-lock-comment-face :weight bold :background ,aero-grey0))))

   ;; js2
   `(js2-warning ((t (:underline (:color ,aero-bright-yellow :style wave)))))
   `(js2-error ((t (:underline (:color ,aero-bright-red :style wave)))))
   `(js2-external-variable ((t (:underline (:color ,aero-normal-blue :style wave)))))
   `(js2-jsdoc-tag ((t (:background unspecified :foreground ,aero-grey2))))
   `(js2-jsdoc-type ((t (:background unspecified :foreground ,aero-grey3))))
   `(js2-jsdoc-value ((t (:background unspecified :foreground ,aero-grey4))))
   `(js2-function-param ((t (:inherit font-lock-variable-name-face :slant italic))))
   `(js2-function-call ((t (:inherit font-lock-function-name-face))))
   `(js2-object-property ((t (:inherit font-lock-variable-name-face))))
   `(js2-instance-member ((t (:background unspecified :foreground ,aero-bright-yellow))))
   `(js2-private-member ((t (:background unspecified :foreground ,aero-bright-yellow))))
   `(js2-private-function-call ((t (:background unspecified :foreground ,aero-normal-blue))))
   `(js2-jsdoc-html-tag-name ((t (:background unspecified :foreground ,aero-grey3))))
   `(js2-jsdoc-html-tag-delimiter ((t (:background unspecified :foreground ,aero-grey4))))

   ;; rjsx
   `(rjsx-attr ((t (:inherit js2-object-property :slant italic))))

   ;; popup
   `(popup-face ((t (:underline nil :foreground ,aero-normal-mint :background ,aero-grey0))))
   `(popup-menu-mouse-face ((t (:underline nil :foreground ,aero-fg :background ,aero-normal-green))))
   `(popup-menu-selection-face ((t (:underline nil :foreground ,aero-fg :background ,aero-normal-green))))
   `(popup-tip-face ((t (:underline nil :foreground ,aero-grey4 :background ,aero-grey1))))

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
   `(counsel-selection ((t (:foreground ,aero-brilliant-white :background ,aero-grey1))))
   `(counsel-selection-line ((t (:foreground ,aero-brilliant-white :background ,aero-grey1))))
   `(counsel-separator ((t (:foreground ,aero-bright-red))))
   `(counsel-source-header ((t (:foreground ,aero-grey4))))
   `(counsel-visible-mark ((t (:foreground ,aero-brilliant-black :background ,aero-grey4))))

   ;; avy
   `(avy-lead-face
     ((t (:foreground ,aero-normal-black :background ,aero-normal-cyan :slant normal))))
   `(avy-lead-face-0
     ((t (:foreground ,aero-normal-black :background ,aero-bright-blue :slant normal))))
   `(avy-lead-face-1
     ((t (:foreground ,aero-normal-white :background ,aero-grey0 :slant normal))))
   `(avy-lead-face-2
     ((t (:foreground ,aero-normal-black :background ,aero-normal-blue :slant normal))))

   ;; hi-lock-mode
   `(hi-black-b ((t (:foreground ,aero-brilliant-black :weight bold))))
   `(hi-black-hb ((t (:foreground ,aero-brilliant-black :weight bold :height 1.5))))
   `(hi-blue ((t (:foreground ,aero-normal-black :background ,aero-normal-blue))))
   `(hi-blue-b ((t (:foreground ,aero-normal-blue :weight bold))))
   `(hi-green ((t (:foreground ,aero-normal-black :background ,aero-normal-green))))
   `(hi-green-b ((t (:foreground ,aero-normal-green :weight bold))))
   `(hi-pink ((t (:foreground ,aero-normal-black :background ,aero-normal-magenta))))
   `(hi-red-b ((t (:foreground ,aero-bright-red :weight bold))))
   `(hi-yellow ((t (:foreground ,aero-normal-black :background ,aero-bright-yellow))))

   ;; company-mode
   `(company-scrollbar-bg ((t (:background ,aero-grey0))))
   `(company-scrollbar-fg ((t (:background ,aero-grey0))))
   `(company-tooltip ((t (:background ,aero-grey0))))
   `(company-tooltip-annotation ((t (:foreground ,aero-normal-green))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation))))
   `(company-tooltip-selection ((t (:foreground ,aero-normal-magenta :background ,aero-grey1))))
   `(company-tooltip-common ((t (:foreground ,aero-normal-blue :underline t))))
   `(company-tooltip-common-selection ((t (:foreground ,aero-normal-blue :underline t))))
   `(company-preview-common ((t (:foreground ,aero-fg))))
   `(company-preview ((t (:background ,aero-normal-blue))))
   `(company-preview-search ((t (:background ,aero-normal-blue))))
   `(company-template-field ((t (:foreground ,aero-brilliant-black :background ,aero-bright-yellow))))
   `(company-echo-common ((t (:foreground ,aero-bright-red))))

   ;; tool tips
   `(tooltip ((t (:foreground ,aero-normal-yellow :background ,aero-grey0))))

   ;; term
   `(term-color-black ((t (:foreground ,aero-grey1 :background ,aero-grey0))))
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
   `(org-block ((t (:inherit fixed-pitch :background ,aero-grey0))))
   `(org-block-begin-line ((t (:background ,aero-grey0))))
   `(org-block-end-line ((t (:background ,aero-grey0))))
   `(org-code ((t (:inherit fixed-pitch :background ,aero-grey0))))
   `(org-column ((t (:background ,aero-normal-black))))
   `(org-column-title ((t (:background ,aero-normal-black :underline t :weight bold))))
   `(org-date ((t (:foreground ,aero-normal-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,aero-bright-red))))
   `(org-document-info ((t (:foreground ,aero-normal-blue))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-document-title ((t (:foreground ,aero-normal-blue :height 1.4 :underline nil :weight bold))))
   `(org-done ((t (:foreground ,aero-normal-blue :weight bold :bold t))))
   `(org-drawer ((t (:inherit font-lock-function-name-face))))
   `(org-ellipsis ((t (:foreground ,aero-grey3))))
   `(org-footnote ((t (:foreground ,aero-normal-blue :underline t))))
   `(org-formula ((t (:foreground ,aero-bright-yellow))))
   `(org-headline-done ((t (:foreground ,aero-normal-blue))))
   `(org-hide ((t (:foreground ,aero-bg))))
   `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   `(org-latex-and-related ((t (:foreground ,aero-normal-blue))))
   `(org-level-1 ((t (:foreground ,aero-normal-blue :height 1.2 :weight bold))))
   `(org-level-2 ((t (:foreground ,aero-bright-yellow :height 1.15 :weight bold))))
   `(org-level-3 ((t (:foreground ,aero-normal-magenta :height 1.1 :weight bold))))
   `(org-level-4 ((t (:foreground ,aero-bright-red :height 1.05 :weight bold))))
   `(org-level-5 ((t (:foreground ,aero-normal-green :weight bold))))
   `(org-level-6 ((t (:foreground ,aero-normal-blue :weight bold))))
   `(org-level-7 ((t (:foreground ,aero-normal-blue :weight bold))))
   `(org-level-8 ((t (:foreground ,aero-bright-yellow :weight bold))))
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
   `(ein:cell-input-prompt ((t (:foreground ,aero-normal-blue :slant italic :box ,aero-grey1))))
   `(ein:cell-output-prompt ((t (:foreground ,aero-normal-cyan :slant italic :box ,aero-grey1))))

   ;; elfeed
   `(elfeed-search-title-face ((t (:foreground ,aero-grey2))))
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

   ;; writegood-mode
   `(writegood-passive-voice-face ((t (:underline (:style wave :color ,aero-normal-blue)))))

   ;; ace-jump-mode
   `(ace-jump-face-background ((t (:foreground ,aero-grey3 :background ,aero-bg :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,aero-bright-red :background ,aero-bg :inverse-video nil))))

   ;; ace-window
   `(aw-background-face ((t (:forground  ,aero-normal-yellow :background ,aero-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:foreground ,aero-bright-red :background ,aero-bg :height 4.0))))

   ;; show-paren
   `(show-paren-match ((t (:background ,aero-grey1 :foreground ,aero-normal-blue :weight bold))))
   `(show-paren-mismatch ((t (:background ,aero-normal-red :foreground ,aero-normal-black :weight bold))))

   ;; swiper
   `(swiper-match-face-1 ((t (:inherit match))))
   `(swiper-match-face-2 ((t (:inherit swiper-match-face-1))))
   `(swiper-match-face-3 ((t (:inherit swiper-match-face-1))))
   `(swiper-match-face-4 ((t (:inherit swiper-match-face-1))))

   ;; ivy
   `(ivy-current-match ((t (:foreground ,aero-bright-cyan :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,aero-bright-yellow))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,aero-bright-orange))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,aero-bright-red))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,aero-bright-magenta))))

   ;; ivy-posframe
   `(ivy-posframe ((t (:background ,aero-normal-black))))
   `(ivy-posframe-border ((t (:box ,aero-normal-red))))

   ;; magit
   `(magit-bisect-bad ((t (:foreground ,aero-bright-red))))
   `(magit-bisect-good ((t (:foreground ,aero-normal-green))))
   `(magit-bisect-skip ((t (:foreground ,aero-bright-yellow))))
   `(magit-blame-heading ((t (:foreground ,aero-fg :background ,aero-grey1))))
   `(magit-branch-local ((t (:foreground ,aero-normal-blue))))
   `(magit-branch-current ((t (:underline ,aero-normal-blue :inherit magit-branch-local))))
   `(magit-branch-remote ((t (:foreground ,aero-normal-magenta))))
   `(magit-cherry-equivalent ((t (:foreground ,aero-normal-magenta))))
   `(magit-cherry-unmatched ((t (:foreground ,aero-normal-blue))))
   `(magit-diff-added ((t (:foreground ,aero-bright-green))))
   `(magit-diff-added-highlight ((t (:foreground ,aero-bright-green :inherit magit-diff-context-highlight))))
   `(magit-diff-base
     ((t (:background ,aero-bright-yellow :foreground ,aero-grey4))))
   `(magit-diff-base-highlight
     ((t (:background ,aero-bright-yellow :foreground ,aero-fg))))
   `(magit-diff-context
     ((t (:foreground ,aero-grey0 :foreground ,aero-fg))))
   `(magit-diff-context-highlight
     ((t (:background ,aero-normal-black :foreground ,aero-fg))))
   `(magit-diff-hunk-heading
     ((t (:background ,aero-grey1 :foreground ,aero-grey4))))
   `(magit-diff-hunk-heading-highlight
     ((t (:background ,aero-grey1 :foreground ,aero-fg))))
   `(magit-diff-hunk-heading-selection
     ((t (:background ,aero-grey1 :foreground ,aero-bright-yellow))))
   `(magit-diff-lines-heading
     ((t (:background ,aero-bright-yellow :foreground ,aero-fg))))
   `(magit-diff-removed
     ((t (:foreground ,aero-bright-red))))
   `(magit-diff-removed-highlight
     ((t (:foreground ,aero-bright-red :inherit magit-diff-context-highlight))))
   `(magit-diffstat-added ((t (:foreground ,aero-bright-green))))
   `(magit-diffstat-removed ((t (:foreground ,aero-bright-red))))
   `(magit-dimmed ((t (:foreground ,aero-grey2))))
   `(magit-hash ((t (:foreground ,aero-normal-green))))
   `(magit-log-author ((t (:foreground ,aero-bright-red))))
   `(magit-log-date ((t (:foreground ,aero-normal-blue))))
   `(magit-log-graph ((t (:foreground ,aero-grey2))))
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
   `(magit-refname ((t (:foreground ,aero-grey3))))
   `(magit-section-heading ((t (:foreground ,aero-bright-yellow :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,aero-bright-yellow))))
   `(magit-section-highlight ((t (:background ,aero-grey6))))
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
   `(git-gutter-fr:added ((,class (:inherit diff-added))))
   `(git-gutter-fr:deleted ((,class (:inherit diff-removed))))
   `(git-gutter-fr:modified ((,class (:inherit diff-changed))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:color ,aero-grey3 :style line)))))
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
   `(font-latex-math-face ((t (:foreground ,aero-grey4))))
   `(font-latex-script-char-face ((t (:foreground ,aero-normal-blue))))
   `(font-latex-sectioning-5-face ((t (:foreground ,aero-bright-yellow :bold t))))
   `(font-latex-sedate-face ((t (:foreground ,aero-grey3))))
   `(font-latex-string-face ((t (:foreground ,aero-bright-yellow))))
   `(font-latex-verbatim-face ((t (:foreground ,aero-grey3))))
   `(font-latex-warning-face ((t (:foreground ,aero-bright-red :weight bold))))
   `(preview-face ((t (:background ,aero-grey0))))

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
   `(undo-tree-visualizer-default-face ((t (:foreground ,aero-grey2))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,aero-bright-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,aero-normal-blue))))

   ;; widget faces
   `(widget-button-pressed-face ((t (:foreground ,aero-bright-red))))
   `(widget-documentation-face ((t (:foreground ,aero-normal-green))))
   `(widget-field ((t (:foreground ,aero-fg :background ,aero-grey1))))
   `(widget-single-line-field ((t (:foreground ,aero-fg :background ,aero-grey1))))

   ;; Solaire
   `(solaire-mode-line-face ((t (:inherit mode-line))))
   `(solaire-mode-line-inactive-face ((t (:inherit mode-line-inactive))))
   `(solaire-default-face ((t (:inherit default :background ,aero-bg))))
   `(solaire-fringe-face ((t (:inherit fringe :background ,aero-bg))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,aero-normal-blue))))
   `(eshell-ls-archive ((t (:foreground ,aero-grey4))))
   `(eshell-ls-backup ((t (:foreground ,aero-grey3))))
   `(eshell-ls-clutter ((t (:foreground ,aero-bright-yellow :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,aero-normal-yellow))))
   `(eshell-ls-executable ((t (:weight bold))))
   `(eshell-ls-missing ((t (:foreground ,aero-bright-red :bold t))))
   `(eshell-ls-product ((t (:foreground ,aero-bright-red))))
   `(eshell-ls-readonly ((t (:foreground ,aero-grey4))))
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

(provide-theme 'aero-dark)
(provide 'aero-dark-theme)
;;; aero-dark-theme.el ends here
