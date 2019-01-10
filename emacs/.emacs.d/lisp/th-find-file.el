(defvar find-file-confirm-size-p t
  "If `find-file' should check if the file might be dangerously large for Emacs to open")

(defvar find-file-confirm-ratio-threshold 500
  "Average line length before `find-file' warns when visiting a file")

(defvar find-file-confirm-size-error-p t
  "If `find-file' should abort immediately when visiting a file larger than the threshold.

Set to `nil' to get a confirmation rather than an error.")

(defun find-file-confirm-size (filename)
  (if (and find-file-confirm-size-p
           (file-exists-p filename)
           (not (file-directory-p filename)))
      ;; If the file exists, we should try to figure out if it might be
      ;; too large or not
      (let* ((size (file-attribute-size (file-attributes filename)))
             ;; Since the entire point of this function is that we can not use
             ;; Emacs builtin facilities, we shell out to `wc' to get the line
             ;; count of the file
             (lines (string-to-number
                     (first
                      (split-string
                       (shell-command-to-string
                        ;; TODO(thiderman): Might not work on other platforms,
                        ;; at least not on Windows. Maybe OSX, I dunno.
                        (format "wc -l %s" filename))))))
             ;; As long as there is more than one line, we are going to check
             ;; the ratio of the file. If there are zero lines (e.g. the file
             ;; ends without a linebreak), fake it to be one since Emacs
             ;; unfortunately still lacks the capability to divide by zero.
             (ratio (/ size (if (zerop lines) 1 lines)))
             (above-threshold (> ratio find-file-confirm-ratio-threshold)))

        (if above-threshold
            (if find-file-confirm-size-error-p
                (error (format "%s has an average line length of %d, which is disallowed for opening. (set `find-file-confirm-size-error-p' to `nil' to override)"
                               filename ratio))
              (yes-or-no-p (format "%s has an average line length of %d. Really open?"
                                   filename ratio)))
          t))
    t))


;; This is copied directly from Emacs 26.1

;;;###autoload
(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type \\[next-history-element] to pull it into the minibuffer.

The first time \\[next-history-element] is used after Emacs prompts for
the file name, the result is affected by `file-name-at-point-functions',
which by default try to guess the file name by looking at point in the
current buffer.  Customize the value of `file-name-at-point-functions'
or set it to nil, if you want only the visited file name and the
current directory to be available on first \\[next-history-element]
request.

You can visit files on remote machines by specifying something
like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
also visit local files as a different user by specifying
/sudo::FILE for the file name.
See the Info node `(tramp)File name Syntax' in the Tramp Info
manual, for more about this.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting `find-file-wildcards' to nil.

To visit a file without any kind of conversion and without
automatically choosing a major mode, use \\[find-file-literally]."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (when (find-file-confirm-size filename)
    (let ((value (find-file-noselect filename nil nil wildcards)))
      (if (listp value)
	        (mapcar 'pop-to-buffer-same-window (nreverse value))
        (pop-to-buffer-same-window value)))))

(provide 'th-find-file)
