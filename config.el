;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

;; ==============================================================================
;; General:
;; ==============================================================================

(setq default-directory "~/")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Joaquín P. Centeno"
      user-mail-address "jpcenteno@users.noreply.github.com")

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

;; Mac OS
;; Right option key for symbols keyboard
(setq ns-right-alternate-modifier 'none)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; Reload the font settings with `(doom/reload-font)'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font
      (font-spec :family "Fira Code" :size 13 :weight 'medium)
      doom-variable-pitch-font
      (font-spec :family "Source Serif Pro" :size 12 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;;
;; I prefer to use the theme-changer package instead.
;;
(use-package! theme-changer
  :init
  (setq calendar-location-name "Buenos Aires")
  (setq calendar-latitude -34.6037)
  (setq calendar-longitude -58.3816)
  (require 'theme-changer)
  :config
  (change-theme 'doom-opera-light 'doom-city-lights))

;;; =============================================================================
;;; Auto save:
;;; =============================================================================

;; `auto-save-mode` creates periodic backup files for buffers.
;;
;; My biggest concern is that autosave might expose encrypted files [1].
;;
;; Beyond that, I don't need this feature, since I save files periodically by
;; hitting `:q`.
;;
;; For more documentation on autosave, see [2].
;;
;; [1]: https://orgmode.org/worg/org-tutorials/encrypting-files.html#org9eb228a
;; [2]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save.html

(setq auto-save-default nil
      auto-save-visited-mode nil)

;; ==============================================================================
;; Text editing:
;; ==============================================================================

;; Show trailing whitespace. Delete trailing whitespace before save.
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable line numbers on the left.
(setq display-line-numbers-type nil)

; Restore substitution behavior on ~s/s~.
(after! evil-snipe (evil-snipe-mode -1))

(map! :after evil
      :nv ";" 'evil-ex
      ;; Prevent evil-emacs-state.
      :nviomrg "C-z" nil)

;; =============================================================================
;; Prose editing & note taking.
;; =============================================================================

;; Wrap text automatically.
;;
;; Learned from [1].
;;
;; [1]: https://www.reddit.com/r/emacs/comments/9td154/is_there_a_way_to_get_better_word_wrapping_in/e8w93d7/
(use-package! auto-fill-mode
  :hook (org-mode markdown-mode))

;; Check spelling while editing code. This mode checks spelling inside comments
;; and strings while writing code.
;;
;; Learned from [1].
;;
;; [1]: https://emacsredux.com/blog/2019/05/24/spell-checking-comments/
(use-package! flyspell-prog-mode
  :hook prog-mode)


(use-package! deft
  :init
  (setq deft-directory "~/org/notes")
  (unless (file-exists-p deft-directory)
    (make-directory deft-directory t)))

(use-package! org
  :init
  (setq org-directory "~/org/"
        org-agenda-files '("~/org/agenda"))
  :config
  (setq

   ;; Don't increase nestedness when adding newline on a list.
   evil-org-special-o/O '(item table-row)

   ;; Don't indent org-mode files. This will make the buffer look more
   ;; prose-like than code-like.
   org-startup-indented nil
   org-indent-mode nil

   ;; Keep 1 blank line after collapsed list elements. Let them breathe.
   org-cycle-separator-lines 1

   ;; Images:
                                        ; Load images by default
   org-startup-with-inline-images t
                                        ; Resize very large images.
   mm-inline-large-images t))

;; Pakcage org-fancy-priorities [1] replaces org-mode priorities with custom
;; strings.
;;
;; I use the same unicode flag symbol for the three priorities, which are then
;; color-coded by the theme I use.
;;
;; [1]: https://github.com/harrybournis/org-fancy-priorities
(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚑" "⚑" "⚑")))

(defun define-org-capture-template (template)
  "Add a org-capture template to org-capture-templates removing previous entries
with the same key."
  (let* ((keys              (car template))
         (has-same-key-p    (lambda (x) (string= keys (car x))))
         (templates-clean   (cl-remove-if has-same-key-p org-capture-templates))
         (templates-updated (cons template templates-clean)))
    (setq org-capture-templates templates-updated)))

(define-org-capture-template
  '("r" "RIL entry" entry (file "~/org/ril.org")
    (file "~/.doom.d/org-capture-templates/ril.org")
    :empty-lines-before 1))

(define-org-capture-template
  '("i" "Idea" entry
    (file "~/org/ideas.org")
    (file "~/.doom.d/org-capture-templates/idea.org")
    :empty-lines-before 1))


;; ==============================================================================
;; Terminal:
;; ==============================================================================

(use-package! vterm
  :init
  ;; Disable `evil-mode' on `vterm-mode'.
  (after! evil
    ;; Send `<escape>' key to the terminal instead of evil mode. Hide the
    ;; buffer by closing the window using `C-x 0'.
    (evil-define-key 'insert vterm-mode-map
      (kbd "<escape>") #'vterm--self-insert)))
