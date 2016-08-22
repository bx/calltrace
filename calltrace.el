(require 'org)
(require 'outline)
(require 'org-archive)

(set-display-table-slot standard-display-table 
                        'selective-display (string-to-vector "{repeated}"))

(setq org-archive-default-command 'org-archive-set-tag)
(setq org-tags-column 1)
(setq org-auto-align-tags nil)

(defun ct-fnregexp (&optional fn entry)
  "Returns a regular expression string that searches for a
headline with function entry or exit information.

 If FN is nil it returns a regular expression that finds any
function entry/exit headline, else it returns a a regular
expression that only matches headlines that contain that function
name (string).

If ENTRY is nil, then it will return a regular expression that
matches both function entries (calls) and exits (returns). If
entry is \">\" (string) then it returns a regular expression that
only matches entries, else it returns a regular expression that
only matches exits."

  (if (null fn)
      (setq fn "\\([-_.[:alnum:]]+\\)"))
  (cond ((null entry)
         (setq entry "\\(>\\|<\\)"))
        ((string entry ">")
         (setq entry "\\(>\\)"))
        (t
         (setq entry "\\(<\\)")))
  (concat "^[[:space:]]*" entry "[[:space:]]*" fn "\\(@0[xX][a-fA-F0-9]+\\)?[[:space:]]*" (regexp-quote "[[")))
  

(defun ct-function-entry-p ()
  "Tests if headline at current point is a function entry,
returns nil if it isn't"

  (let ((res (ct-get-function-match 1)))
    (string= res ">")))

(defun ct-function-entry-exit-p ()
  "Tests if headline at current point is a function
entry/exit. Returns nil if it isn't."
  (not (null (ct-get-function-match 0 t))))

(defun ct-get-function-match (n &optional noerror)
  "Tests if headline at current point matches current is a
function and returns the Nth group of the the ct-fnregexp.  If
NOERROR is nil, it signals an error if a mach is not found else
it returns nil"
  (let* ((h (org-element-at-point))
         (title (org-element-property :title h))         
         (res nil)
         (match nil)
         (re (ct-fnregexp)))
    (with-temp-buffer
      (insert title)
      (goto-char (point-min))
      (setq match (list
                   (re-search-forward re nil t)
                   (match-string n)))
      (if (null (car match))
          (if (null noerror)
              (error "No function name here")
            nil))
      (nth 1 match))))

(defun ct-next-function-headline (&optional visible-only)
  "Moves the current point to the next headline. If VISIBLE-ONLY
is not nil, it will only move the point to a currently visible
headline"
  (interactive)
  (let ((curr (point))
        (next (if visible-only (outline-next-visible-heading 1) (outline-next-heading)))
        (res nil))
    (while (and next (null res) (< (point) (point-max)))
      (if (ct-function-entry-exit-p)
          (setq res (point))
        (setq next (if visible-only (outline-next-visible-heading 1) (outline-next-heading)))))
    (if (= (point-max) (point))
        nil
      (point))))
  
(defun ct-function-name-at-current-headline ()
  "Returns the function name at the current headline, raises
error if it is not at a function headline"
  (let ((fn (ct-get-function-match 2)))
    fn))

(defun ct-set-to-hide (&optional unhide donthide)
  "Sets a flag to hide this function heading. If UNHIDE is not
nil then it unsets the hide flag. If DONTHIDE is not nil then it
doesn't perform an action that actually hides the subtree"
  (let ((a nil))
    (save-excursion
      (setq a (ct-is-hidden))
      (if unhide
          (progn
            (if a
                (progn
                  (org-back-to-heading t)                 
                  (org-toggle-tag org-archive-tag)))
            (if (null donthide)
                (org-show-context 'occur-tree)))
          (progn
            (if (null a)
                (progn
                  (org-back-to-heading t)
                  (org-toggle-archive-tag)
		  (if (null donthide)
		      (hide-subtree)))))))))

(defun ct-remove-all-hide-tags ()
  "Removes all tags that calltrace set to hide functions"
  (interactive)
  (org-map-entries (lambda ()
		     (org-set-tags-to (remove org-archive-tag (org-get-tags-at))))
		   nil 'file))

(defun ct-is-hidden ()
  "Checks if headline at current point is hidden"
  (not (null (member org-archive-tag (org-get-tags-at)))))

(defun ct-mark-dups (&optional visibleonly)
  "Marks any repeated adjacent function calls to be hidden. If
VISIBLEONLY is not nill, then it only operates on currently
visible headlines. Does not hide anything."
  (interactive)  
  (let ((prevfun nil)
	(currfun nil)
	(llast 0)
	(next t)
	(hide nil)
	(stack nil)
	(hasdups nil)
	(parent-is-hidden nil)
	(maxline (line-number-at-pos (point-max)))
	lspos level i)
    (save-excursion
      (goto-char (point-min))
      (if (null (ct-function-entry-exit-p))
	  (ct-next-function-headline visibleonly))
      (while next      
	(setq hide nil)
	(setq lspos (line-number-at-pos (point)))
	(message "(mark) processing line %s of %s" lspos maxline)	
	(setq currfun (ct-function-name-at-current-headline))
	(setq level (org-reduced-level (org-outline-level)))
	(if (ct-function-entry-p)
	    (progn
	      (if (not (equal level llast))
		  (setq prevfun nil)
		(if (or (string= prevfun currfun) parent-is-hidden)
		    (setq hide t)))
	      (push currfun stack))
	  (progn ; function exit
	    (pop stack)
	    (if (and (< level llast) (> llast 0))
		(dotimes (i (- (- llast level) 1))
		  (pop stack))) ; pop the extra frames off
	    (org-backward-heading-same-level 1 (not visibleonly)) ;check if entry is hidden
	    (setq hide (and (ct-function-entry-exit-p) (ct-function-entry-p) (string= currfun (ct-function-name-at-current-headline)) (ct-is-hidden)))
	    (org-forward-heading-same-level 1 (not visibleonly)))) ;go back to origional location
	(setq llast level)
	(setq prevfun currfun)
	(ct-set-to-hide (not hide) t)
	(setq hasdups (or hasdups hide))
	(setq next (ct-next-function-headline visibleonly))))
    hasdups))

(defun ct-collapse-dups-until-none (&optional savetags)
  "Marks duplicates to be hidden and then iteratively hides
repeated adjacent function calls until there are none. If
SAVETAGS is not nil, it does not remove existing hide tags."
  (interactive)
  (outline-show-all)
  (if savetags
      (ct-coalesce-hidden)    
    (let ((haddups t)
	  (i 0))
      (while haddups
	(message "collapse iteration %s" i)
	(setq haddups (ct-mark-dups t))
	(setq i (+ i 1))
	(ct-hide-hidden)))
    (org-remove-occur-highlights t)
    (ct-coalesce-hidden)))
  
(defun ct-collapse-dups ()
  "Marks duplicates to be hidden and then hides current set of
repeated adjacent function calls."
  (interactive)
  (ct-remove-all-hide-tags)
  (ct-mark-dups)
  (ct-hide-hidden)
  (org-remove-occur-highlights t)
  (ct-coalesce-hidden))


(defun ct-get-unique-functions-after-point ()
  "Prints out the set of functions that are called after (and
including) the current headline."
  (interactive)
  (let ((oldpoint (point))
	(funs nil)
	(funname nil)
	(next t))
    (while next
      (when (ct-function-entry-exit-p)
	(setq funname (ct-function-name-at-current-headline))
	(if (null (member funname funs))
	    (push funname funs)))
      (setq next (ct-next-function-headline t)))
    (message (concat "{" (mapconcat 'identity funs ", ") "}"))
    funs))

(defun ct-hide-hidden ()
  "Does the work to hide headlines that have been marked to
hide."
  (let ((next t)
	(maxline (line-number-at-pos (point-max))))
      (show-all)
      (goto-char (point-min))
      (while next
	(message "(hide?) processing line %s of %s" (line-number-at-pos) maxline)
	(if (ct-is-hidden)
	    (outline-flag-region (line-beginning-position) (line-end-position) t)
	  (progn
	    (outline-flag-region (line-end-position) (line-end-position) nil)
	    (beginning-of-line)))
	(setq next (ct-next-function-headline)))))


(defun ct-coalesce-hidden ()
  "Merges together adjacent hidden lines"
  (interactive)
  (let ((next t)
	(hstart -1)
	(hend -1)
	(maxline (line-number-at-pos (point-max))))
    (goto-char (point-min))
    (while next
      (message "(coalesce?) processing line %s of %s" (line-number-at-pos) maxline)
      (if (or (outline-invisible-p) (ct-is-hidden))
	  (progn
	    (if (= hstart -1)
		(setq hstart (point)))
	    (setq hend (line-end-position)))
	(progn
	  (if (> hend (point-min))
	      (progn
		(outline-flag-region hstart hend t)
		(setq hstart -1)
		(setq hend -1)))))
      (setq next (ct-next-function-headline)))))

(provide 'calltrace)
