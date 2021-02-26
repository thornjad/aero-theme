;; aero-light-theme.el --- A practical light theme -*- lexical-binding: t -*-
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2019-2021 Jade Michael Thornton
;; URL: https://gitlab.com/thornjad/aero-theme
;; Version: 1.0.0
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
;; Either way, activate the theme with `(load-theme 'aero-light)' or
;; `(load-theme 'aero-dark)', et voilà!.

;;; Code:
