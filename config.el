;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Joaquín P. Centeno"
      user-mail-address "jpcenteno@users.noreply.github.com")

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

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
      (font-spec :family "Source Code Pro" :size 13 :weight 'medium)
      doom-variable-pitch-font
      (font-spec :family "Source Serif Pro" :size 12 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;;
;; I prefer to use the theme-changer package instead.
(use-package! theme-changer
  :init
  (setq calendar-location-name "Buenos Aires")
  (setq calendar-latitude -34.6037)
  (setq calendar-longitude -58.3816)
  (require 'theme-changer)
  :config
  (change-theme 'doom-opera-light 'doom-city-lights))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(after! deft
  (setq deft-directory "~/org/notes")
  (unless (file-exists-p deft-directory)
    (make-directory deft-directory t)))

;; org mode
(after! org
  (setq org-directory "~/org")
  (setq org-agenda-files
        (nconc
         (file-expand-wildcards
          (concat (file-name-as-directory org-directory) "*.org"))
         (file-expand-wildcards
          (concat (file-name-as-directory org-directory) "projects/*.org"))))

  ;; Keep 1 blank line after collapsed list elements. Let them breathe.
  (setq org-cycle-separator-lines 1))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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

;; Evil mode:

; Restore substitution behavior on ~s/s~.
(after! evil-snipe (evil-snipe-mode -1))
(map! :after evil
      :nv ";" 'evil-ex
      :nv "C-h" 'evil-ex-nohighlight
      ;; Prevent evil-emacs-state.
      :nviomrg "C-z" nil)

;; Mac OS
;; Right option key for symbols keyboard
(setq ns-right-alternate-modifier 'none)

;; -----------------------------------------------------------------------------
;; Lang - Elixir
;; -----------------------------------------------------------------------------

(after! elixir-mode
  ;; Create a buffer-local hook to run elixir-format on save, only when we
  ;; enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))
