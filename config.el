;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name ""
      user-mail-address "")

(defun copy-buffer-filename ()
  "Copy the `buffer-file-name' to the `kill-ring'."
  (interactive)
  (let ((buffer-full-path (buffer-file-name)))
    (message "\"%s\" copied to the kill-ring." buffer-full-path)
    (kill-new buffer-full-path)))

;; -----------------------------------------------------------------------------
;; Aesthetic
;; -----------------------------------------------------------------------------

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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
(setq doom-font (font-spec :family "JetBrains Mono" :size 14))
;doom-variable-pitch-font (font-spec :family "Dank Mono" :size 13)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Doom modeline
(setq

 ;; the "modeline bar" is a little bar on the left that indicates the
 doom-modeline-bar nil

 doom-modeline-modal-icon nil)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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

;; -----------------------------------------------------------------------------
;; Evil Mode.
;; -----------------------------------------------------------------------------

(map! :after evil
      :nv ";" 'evil-ex
      ;; Prevent evil-emacs-state.
      :nviomrg "C-z" nil)

(after! smart-parens
  (sp-with-modes 'org-mode
    ;; Code
    (sp-local-pair "~" "~")
    ;; Verbatim
    (sp-local-pair "=" "=")
    ;; strike-through
    (sp-local-pair "+" "+")
    ;; Underlined
    (sp-local-pair "_" "_")
    ;; Italic
    (sp-local-pair "/" "/")))
