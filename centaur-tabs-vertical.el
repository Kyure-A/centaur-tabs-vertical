;;; centaur-tabs-vertical.el --- Vertical side tabs for centaur-tabs -*- lexical-binding: t; -*-

;; Author: centaur-tabs-vertical contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (centaur-tabs "3.3"))
;; Keywords: convenience, frames
;; URL: https://github.com/Kyure-A/centaur-tabs-vertical

;;; Commentary:
;; Provide vertical side tabs for centaur-tabs, rendered in a side window.

;;; Code:

(require 'cl-lib)
(require 'centaur-tabs)

(defgroup centaur-tabs-vertical nil
  "Vertical side tabs for centaur-tabs."
  :group 'centaur-tabs)

(defcustom centaur-tabs-vertical-positions '(left)
  "Sides to display the vertical tab list."
  :type '(set (const left) (const right))
  :group 'centaur-tabs-vertical)

(defcustom centaur-tabs-vertical-left-width 30
  "Initial width of the left vertical tab list."
  :type 'integer
  :group 'centaur-tabs-vertical)

(defcustom centaur-tabs-vertical-right-width 30
  "Initial width of the right vertical tab list."
  :type 'integer
  :group 'centaur-tabs-vertical)

(defcustom centaur-tabs-vertical-min-width 18
  "Minimum width for the vertical tab list."
  :type 'integer
  :group 'centaur-tabs-vertical)

(defcustom centaur-tabs-vertical-max-width 60
  "Maximum width for the vertical tab list."
  :type 'integer
  :group 'centaur-tabs-vertical)

(defcustom centaur-tabs-vertical-show-icons t
  "Show buffer icons in the vertical tab list."
  :type 'boolean
  :group 'centaur-tabs-vertical)

(defcustom centaur-tabs-vertical-show-group t
  "Show the current group name at the top of the list."
  :type 'boolean
  :group 'centaur-tabs-vertical)

(defcustom centaur-tabs-vertical-show-modified-marker t
  "Show the modified marker for modified buffers."
  :type 'boolean
  :group 'centaur-tabs-vertical)

(defface centaur-tabs-vertical-group-face
  '((t (:inherit centaur-tabs-unselected :weight bold)))
  "Face for the group header line."
  :group 'centaur-tabs-vertical)

(defface centaur-tabs-vertical-handle-face
  '((t (:inherit shadow)))
  "Face for the resize handle."
  :group 'centaur-tabs-vertical)

(defconst centaur-tabs-vertical--buffer-prefix " *centaur-tabs-vertical-")
(defconst centaur-tabs-vertical--known-sides '(left right))

(defvar centaur-tabs-vertical--last-main-window nil)
(defvar centaur-tabs-vertical--refreshing nil)
(defvar centaur-tabs-vertical--advice-installed nil)
(defvar centaur-tabs-vertical--centaur-tabs-managed nil)

(defvar centaur-tabs-vertical-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'centaur-tabs-vertical-mouse-select)
    (define-key map [mouse-2] #'centaur-tabs-vertical-mouse-close)
    map)
  "Mouse keymap for vertical tab entries.")

(defvar centaur-tabs-vertical-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'centaur-tabs-vertical-mouse-close)
    (define-key map [mouse-2] #'centaur-tabs-vertical-mouse-close)
    map)
  "Mouse keymap for close buttons in vertical tabs.")

(defvar centaur-tabs-vertical-resize-map
  (let ((map (make-sparse-keymap)))
    (define-key map [drag-mouse-1] #'centaur-tabs-vertical-mouse-resize)
    map)
  "Mouse keymap for the resize handle.")

(defvar centaur-tabs-vertical-tablist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'centaur-tabs-vertical-select)
    (define-key map (kbd "k") #'centaur-tabs-vertical-close)
    (define-key map (kbd "g") #'centaur-tabs-vertical-refresh)
    (define-key map (kbd "s") #'centaur-tabs-switch-group)
    map)
  "Keymap for `centaur-tabs-vertical-tablist-mode'.")

(define-derived-mode centaur-tabs-vertical-tablist-mode special-mode "CTabs-V"
  "Major mode for vertical centaur-tabs lists."
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local cursor-type nil)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local window-size-fixed nil))

(defun centaur-tabs-vertical--buffer-name (side)
  "Return the buffer name for SIDE."
  (format "%s%s*" centaur-tabs-vertical--buffer-prefix side))

(defun centaur-tabs-vertical--side-width (side)
  "Return the configured width for SIDE."
  (if (eq side 'left)
      centaur-tabs-vertical-left-width
    centaur-tabs-vertical-right-width))

(defun centaur-tabs-vertical--set-side-width (side width)
  "Set WIDTH for SIDE."
  (if (eq side 'left)
      (setq centaur-tabs-vertical-left-width width)
    (setq centaur-tabs-vertical-right-width width)))

(defun centaur-tabs-vertical--side-window-p (window)
  "Return non-nil if WINDOW is a centaur-tabs vertical side window."
  (memq (window-parameter window 'centaur-tabs-vertical-side) '(left right)))

(defun centaur-tabs-vertical--track-main-window (&rest _)
  "Track the last selected non-side window."
  (let ((win (selected-window)))
    (unless (centaur-tabs-vertical--side-window-p win)
      (setq centaur-tabs-vertical--last-main-window win))))

(defun centaur-tabs-vertical--target-window ()
  "Return the target window for tab selection."
  (or (and (window-live-p centaur-tabs-vertical--last-main-window)
           centaur-tabs-vertical--last-main-window)
      (cl-find-if (lambda (win)
                    (not (centaur-tabs-vertical--side-window-p win)))
                  (window-list nil 'no-minibuffer (selected-frame)))))

(defun centaur-tabs-vertical--current-tabset ()
  "Return the current tabset based on the target window buffer."
  (let* ((target (centaur-tabs-vertical--target-window))
         (buf (and (window-live-p target) (window-buffer target))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (centaur-tabs-current-tabset t)))))

(defun centaur-tabs-vertical--tab-label (tab)
  "Return the label string for TAB."
  (if centaur-tabs-tab-label-function
      (funcall centaur-tabs-tab-label-function tab)
    (buffer-name (centaur-tabs-tab-value tab))))

(defun centaur-tabs-vertical--tab-face (selected modified)
  "Return the face for SELECTED and MODIFIED state."
  (cond (selected
         (if modified
             'centaur-tabs-selected-modified
           'centaur-tabs-selected))
        (modified 'centaur-tabs-unselected-modified)
        (t 'centaur-tabs-unselected)))

(defun centaur-tabs-vertical--resize-handle (_side)
  "Return a resize handle string."
  (propertize "|"
              'face 'centaur-tabs-vertical-handle-face
              'mouse-face 'highlight
              'help-echo "Drag to resize"
              'local-map centaur-tabs-vertical-resize-map))

(defun centaur-tabs-vertical--pad (text width face)
  "Pad TEXT to WIDTH using FACE.
If TEXT is longer than WIDTH, truncate it."
  (let ((w (string-width text)))
    (cond
     ((> w width)
      (truncate-string-to-width text width 0 nil t))
     ((< w width)
      (concat text (propertize (make-string (- width w) ? ) 'face face)))
     (t text))))

(defun centaur-tabs-vertical--apply-tab-props (text tab buffer)
  "Apply tab properties to TEXT for TAB and BUFFER."
  (let ((props (list 'centaur-tabs-tab tab
                     'local-map centaur-tabs-vertical-tab-map
                     'mouse-face 'highlight
                     'help-echo (or (buffer-file-name buffer)
                                    (buffer-name buffer)))))
    (add-text-properties 0 (length text) props text)
    text))

(defun centaur-tabs-vertical--apply-close-props (text tab start end)
  "Apply close-button properties to TEXT from START to END for TAB."
  (add-text-properties
   start end
   (list 'centaur-tabs-tab tab
         'local-map centaur-tabs-vertical-close-map
         'mouse-face 'centaur-tabs-close-mouse-face
         'help-echo "Close buffer")
   text)
  text)

(defun centaur-tabs-vertical--render-tab (tab selected side width)
  "Render TAB with SELECTED state for SIDE within WIDTH."
  (let* ((buffer (centaur-tabs-tab-value tab))
         (selected-p (eq tab selected))
         (modified-p (with-current-buffer buffer
                       (and (not buffer-read-only)
                            (buffer-modified-p buffer))))
         (face (centaur-tabs-vertical--tab-face selected-p modified-p))
         (close-face (if selected-p
                         'centaur-tabs-close-selected
                       'centaur-tabs-close-unselected))
         (close-left (if centaur-tabs-set-left-close-button
                         (propertize centaur-tabs-close-button 'face close-face)
                       ""))
         (close-right (if centaur-tabs-set-close-button
                          (propertize centaur-tabs-close-button 'face close-face)
                        ""))
         (close-left-gap (if (> (length close-left) 0) " " ""))
         (close-right-gap (if (> (length close-right) 0) " " ""))
         (icon (if (and centaur-tabs-vertical-show-icons
                        centaur-tabs-set-icons
                        (not centaur-tabs--buffer-show-groups))
                   (centaur-tabs-icon tab face selected-p)
                 ""))
         (marker (if (and centaur-tabs-vertical-show-modified-marker
                          centaur-tabs-set-modified-marker
                          modified-p)
                     centaur-tabs-modified-marker
                   ""))
         (icon-width (string-width icon))
         (marker-width (string-width marker))
         (close-left-width (string-width close-left))
         (close-right-width (string-width close-right))
         (close-left-gap-width (string-width close-left-gap))
         (close-right-gap-width (string-width close-right-gap))
         (gap (if (> icon-width 0) " " ""))
         (gap-width (length gap))
         (fixed-width (+ icon-width
                         gap-width
                         close-left-width
                         close-right-width
                         close-left-gap-width
                         close-right-gap-width))
         (available (max 0 (- width fixed-width)))
         (label-width (max 0 (- available marker-width)))
         (label (centaur-tabs-vertical--tab-label tab))
         (label (if (> label-width 0)
                    (truncate-string-to-width label label-width 0 nil t)
                  ""))
         (label (propertize label 'face face))
         (marker (propertize marker 'face face))
         (label-marker (centaur-tabs-vertical--pad (concat label marker)
                                                   available
                                                   face))
         (content (concat close-left
                          close-left-gap
                          icon
                          gap
                          label-marker
                          close-right-gap
                          close-right))
         (close-left-range (and (> (length close-left) 0)
                                (cons 0 (length close-left))))
         (close-right-range (and (> (length close-right) 0)
                                 (let ((end (length content)))
                                   (cons (- end (length close-right)) end))))
         (content (centaur-tabs-vertical--apply-tab-props content tab buffer)))
    (when close-left-range
      (centaur-tabs-vertical--apply-close-props
       content tab (car close-left-range) (cdr close-left-range)))
    (when close-right-range
      (centaur-tabs-vertical--apply-close-props
       content tab (car close-right-range) (cdr close-right-range)))
    (if (eq side 'right)
        (concat (centaur-tabs-vertical--resize-handle side) content)
      (concat content (centaur-tabs-vertical--resize-handle side)))))

(defun centaur-tabs-vertical--render-header (title side width)
  "Render a group TITLE for SIDE within WIDTH."
  (let* ((label (propertize title 'face 'centaur-tabs-vertical-group-face))
         (label (centaur-tabs-vertical--pad label width 'centaur-tabs-vertical-group-face)))
    (if (eq side 'right)
        (concat (centaur-tabs-vertical--resize-handle side) label)
      (concat label (centaur-tabs-vertical--resize-handle side)))))

(defun centaur-tabs-vertical--ensure-window (side)
  "Ensure a side window exists for SIDE and return it."
  (let* ((buffer (get-buffer-create (centaur-tabs-vertical--buffer-name side)))
         (window (get-buffer-window buffer)))
    (unless (window-live-p window)
      (setq window (display-buffer-in-side-window
                    buffer
                    `((side . ,side)
                      (slot . 0)
                      (window-width . ,(centaur-tabs-vertical--side-width side))
                      (window-parameters . ((no-delete-other-windows . t)
                                            (no-other-window . t)
                                            (centaur-tabs-vertical-side . ,side))))))
      (set-window-dedicated-p window t))
    (set-window-parameter window 'centaur-tabs-vertical-side side)
    (with-current-buffer buffer
      (centaur-tabs-vertical-tablist-mode))
    window))

(defun centaur-tabs-vertical--render (side)
  "Render the vertical tab list for SIDE."
  (let* ((window (centaur-tabs-vertical--ensure-window side))
         (width (max 2 (window-width window)))
         (content-width (max 1 (- width 1)))
         (tabset (centaur-tabs-vertical--current-tabset))
         (tabs (and tabset (centaur-tabs-tabs tabset)))
         (selected (and tabset (centaur-tabs-selected-tab tabset)))
         (header (if tabset
                     (format " %s" (centaur-tabs-2str tabset))
                   " No Tabs")))
    (with-current-buffer (window-buffer window)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when centaur-tabs-vertical-show-group
          (insert (centaur-tabs-vertical--render-header header side content-width))
          (insert "\n"))
        (dolist (tab tabs)
          (insert (centaur-tabs-vertical--render-tab tab selected side content-width))
          (insert "\n"))
        (goto-char (point-min))))))

(defun centaur-tabs-vertical--cleanup-windows (&optional force)
  "Remove windows and buffers for sides not enabled.
If FORCE is non-nil, remove all vertical side windows."
  (dolist (side centaur-tabs-vertical--known-sides)
    (when (or force (not (memq side centaur-tabs-vertical-positions)))
      (let* ((buf (get-buffer (centaur-tabs-vertical--buffer-name side)))
             (win (and buf (get-buffer-window buf))))
        (when (window-live-p win)
          (delete-window win))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(defun centaur-tabs-vertical-refresh (&optional _)
  "Refresh all vertical tab lists."
  (interactive)
  (when (and centaur-tabs-vertical-mode
             (not centaur-tabs-vertical--refreshing))
    (let ((centaur-tabs-vertical--refreshing t))
      (centaur-tabs-vertical--cleanup-windows)
      (dolist (side centaur-tabs-vertical-positions)
        (centaur-tabs-vertical--render side)))))

(defun centaur-tabs-vertical--tab-at-point ()
  "Return the tab at point."
  (get-text-property (point) 'centaur-tabs-tab))

(defun centaur-tabs-vertical--tab-from-event (event)
  "Return the tab from mouse EVENT."
  (let* ((pos (event-start event))
         (pt (or (posn-point pos)
                 (cdr (posn-string pos)))))
    (when (and pt (integerp pt))
      (with-current-buffer (window-buffer (posn-window pos))
        (get-text-property pt 'centaur-tabs-tab)))))

(defun centaur-tabs-vertical--with-target-window (fn tab)
  "Call FN with TAB in the target window."
  (let ((win (centaur-tabs-vertical--target-window)))
    (when (window-live-p win)
      (with-selected-window win
        (funcall fn tab)))))

(defun centaur-tabs-vertical-select ()
  "Select the tab at point."
  (interactive)
  (let ((tab (centaur-tabs-vertical--tab-at-point)))
    (when tab
      (centaur-tabs-vertical--with-target-window #'centaur-tabs-buffer-select-tab tab)
      (centaur-tabs-vertical-refresh))))

(defun centaur-tabs-vertical-close ()
  "Close the tab at point."
  (interactive)
  (let ((tab (centaur-tabs-vertical--tab-at-point)))
    (when tab
      (centaur-tabs-vertical--with-target-window #'centaur-tabs-buffer-close-tab tab)
      (centaur-tabs-vertical-refresh))))

(defun centaur-tabs-vertical-mouse-select (event)
  "Select the tab clicked in EVENT."
  (interactive "e")
  (let ((tab (centaur-tabs-vertical--tab-from-event event)))
    (when tab
      (centaur-tabs-vertical--with-target-window #'centaur-tabs-buffer-select-tab tab)
      (centaur-tabs-vertical-refresh))))

(defun centaur-tabs-vertical-mouse-close (event)
  "Close the tab clicked in EVENT."
  (interactive "e")
  (let ((tab (centaur-tabs-vertical--tab-from-event event)))
    (when tab
      (centaur-tabs-vertical--with-target-window #'centaur-tabs-buffer-close-tab tab)
      (centaur-tabs-vertical-refresh))))

(defun centaur-tabs-vertical--resize-window (window side delta)
  "Resize WINDOW on SIDE by DELTA columns."
  (let* ((current (window-width window))
         (target (+ current delta))
         (target (max centaur-tabs-vertical-min-width target))
         (target (min centaur-tabs-vertical-max-width target))
         (final (- target current)))
    (when (/= final 0)
      (condition-case nil
          (window-resize window final t)
        (error nil))
      (centaur-tabs-vertical--set-side-width side target)
      (centaur-tabs-vertical-refresh))))

(defun centaur-tabs-vertical-mouse-resize (event)
  "Resize the side window using mouse EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (end (event-end event))
         (window (posn-window start))
         (side (window-parameter window 'centaur-tabs-vertical-side)))
    (when (and (window-live-p window) side)
      (let* ((dx (- (car (posn-x-y end)) (car (posn-x-y start))))
             (delta (round (/ (float dx) (frame-char-width))))
             (delta (if (eq side 'right) (- delta) delta)))
        (centaur-tabs-vertical--resize-window window side delta)))))

(defun centaur-tabs-vertical--on-window-selection-change (&rest _)
  "Handle window selection changes."
  (centaur-tabs-vertical--track-main-window)
  (centaur-tabs-vertical-refresh))

(defun centaur-tabs-vertical--on-window-size-change (frame)
  "Track size changes in FRAME."
  (dolist (win (window-list frame 'no-minibuffer))
    (when (centaur-tabs-vertical--side-window-p win)
      (centaur-tabs-vertical--set-side-width
       (window-parameter win 'centaur-tabs-vertical-side)
       (window-width win))))
  (centaur-tabs-vertical-refresh))

(defun centaur-tabs-vertical--advice-line (orig &rest args)
  "Hide horizontal tabs while vertical tabs are active."
  (if centaur-tabs-vertical-mode
      nil
    (apply orig args)))

(defun centaur-tabs-vertical--install-hooks ()
  "Install hooks for vertical tabs."
  (add-hook 'buffer-list-update-hook #'centaur-tabs-vertical-refresh)
  (add-hook 'window-buffer-change-functions #'centaur-tabs-vertical-refresh)
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions
              #'centaur-tabs-vertical--on-window-selection-change))
  (when (boundp 'window-size-change-functions)
    (add-hook 'window-size-change-functions
              #'centaur-tabs-vertical--on-window-size-change))
  (advice-add 'centaur-tabs-display-update :after #'centaur-tabs-vertical-refresh)
  (centaur-tabs-vertical--track-main-window))

(defun centaur-tabs-vertical--remove-hooks ()
  "Remove hooks for vertical tabs."
  (remove-hook 'buffer-list-update-hook #'centaur-tabs-vertical-refresh)
  (remove-hook 'window-buffer-change-functions #'centaur-tabs-vertical-refresh)
  (when (boundp 'window-selection-change-functions)
    (remove-hook 'window-selection-change-functions
                 #'centaur-tabs-vertical--on-window-selection-change))
  (when (boundp 'window-size-change-functions)
    (remove-hook 'window-size-change-functions
                 #'centaur-tabs-vertical--on-window-size-change))
  (advice-remove 'centaur-tabs-display-update #'centaur-tabs-vertical-refresh))

(defun centaur-tabs-vertical--install-advice ()
  "Install advice to hide horizontal tabs."
  (unless centaur-tabs-vertical--advice-installed
    (advice-add 'centaur-tabs-line :around #'centaur-tabs-vertical--advice-line)
    (setq centaur-tabs-vertical--advice-installed t)))

(defun centaur-tabs-vertical--remove-advice ()
  "Remove advice that hides horizontal tabs."
  (when centaur-tabs-vertical--advice-installed
    (advice-remove 'centaur-tabs-line #'centaur-tabs-vertical--advice-line)
    (setq centaur-tabs-vertical--advice-installed nil)))

(defun centaur-tabs-vertical--enable ()
  "Enable vertical tabs."
  (setq centaur-tabs-vertical--centaur-tabs-managed (not centaur-tabs-mode))
  (unless centaur-tabs-mode
    (centaur-tabs-mode 1))
  (centaur-tabs-vertical--install-advice)
  (centaur-tabs-vertical--install-hooks)
  (centaur-tabs-vertical-refresh))

(defun centaur-tabs-vertical--disable ()
  "Disable vertical tabs."
  (centaur-tabs-vertical--remove-hooks)
  (centaur-tabs-vertical--remove-advice)
  (centaur-tabs-vertical--cleanup-windows t)
  (when centaur-tabs-vertical--centaur-tabs-managed
    (centaur-tabs-mode -1))
  (setq centaur-tabs-vertical--centaur-tabs-managed nil)
  (force-window-update))

;;;###autoload
(defun centaur-tabs-vertical-enable ()
  "Enable vertical tabs."
  (interactive)
  (centaur-tabs-vertical-mode 1))

;;;###autoload
(defun centaur-tabs-vertical-disable ()
  "Disable vertical tabs."
  (interactive)
  (centaur-tabs-vertical-mode 0))

;;;###autoload
(define-minor-mode centaur-tabs-vertical-mode
  "Toggle vertical centaur-tabs side windows."
  :global t
  :group 'centaur-tabs-vertical
  (if centaur-tabs-vertical-mode
      (centaur-tabs-vertical--enable)
    (centaur-tabs-vertical--disable)))

(provide 'centaur-tabs-vertical)

;;; centaur-tabs-vertical.el ends here
