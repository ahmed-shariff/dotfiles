(in-package :stumpwm)
(set-prefix-key (kbd "C-z"))

(ql:quickload '(:cl-utilities))
;		:cl-emoji))
;; change the prefix key to something else

(set-module-dir "~/.stumpwm.d/modules")

(run-shell-command "feh --randomize --bg-fill /media/Files/Pictures/Background/*")
(run-shell-command "xcompmgr -c &")

(define-key *root-map* (kbd "c") "exec tilix")
(define-key *root-map* (kbd "e") "exec (unset SBCL_HOME;emacs &)")
(define-key *root-map* (kbd "w") "frame-windowlist")
(define-key *root-map* (kbd ".") "mymenu")

(run-commands "mode-line")
(setf *message-window-padding* 150)
(setf *message-window-gravity* :top)
(setf *input-window-gravity* :top)


(set-fg-color "#ffad29")
(set-bg-color "#253142")
(set-font "-misc-dejavu sans-medium-r-normal--0-0-0-0-p-0-iso8859-15")
(set-border-color "#1f3459")
(set-msg-border-width 3)

(load-module "wifi")

(defvar *power-status* "upower -i $(upower -e | grep BAT) |grep -E \"state|to\ empty|to\ full|percentage\"")


(defcommand mymenu () ()
  (labels ((pick (options)
	     (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
	       (cond
		 ((null selection)
		  (throw 'stumpwm::error "Abort."))
		 ((stringp (second selection))
		  (second selection))
		 ((consp (cadr selection))
		  (pick (cdr selection)))
		 (t
		  (cddr selection))))))
    (let ((choice (pick *app-menu*)))
      (if (consp choice)
	  (run-shell-command (caar choice) (cdar choice));(run-shell-command (car a))
	  (run-shell-command choice)))))
(defparameter *app-menu* '(("Opera" "opera")
			   ("Media"
					;sub-menu
			    ("Music" "vlc /media/Files/Music")
			    ("Open media folder" "thunar /media/Files/Media")
			    ("VLC player" "vlc"))
			   ("File Manager" "thunar")
			   ("Check power status" t (*power-status* t)) ;; "upower -i $(upower -e | grep BAT) |grep -E \"state|to\ empty|to\ full|percentage\""
			   ("Change background" "feh --randomize --bg-fill /media/Files/Pictures/Background/*")))
							
(defun shell-command-fn (command) "Run a shell command and display output to screen.
    This must be used in a functional side-effects-free style! If a program does not
    exit of its own accord, Stumpwm might hang!"
  (let ((s (run-shell-command command t)))))    

(defcommand shell-command (command)
  ((:string "sh: "))
  (shell-command-fn command));

(defun battery-status ()
  (flet ((get-percentage-string (seq)
	   (remove #\Space
		   (second (cl-utilities:split-sequence
			    (character ":")
			    (find "percentage" seq :test #'search)))
		   :test #'char=))
	 (get-time-string (seq)
	   (let ((val (find "time" seq :test #'search)))
	     (if val
	 	 (cl-ppcre:regex-replace
		  "minutes"
		  (cl-ppcre:regex-replace
		   "hours"
		   (remove #\Space
			   (second
			    (cl-utilities:split-sequence (character ":") val))
			   :test #'char=)
		   "h")
		  "m")
	 	 nil))))
    (let* ((b (run-shell-command *power-status* t))
	   (c (cl-utilities:split-sequence #\Newline b))
	   (d (if (search "discharging" b)
		  (concatenate 'string "D "
			       (get-time-string c)
			       " "
			       (get-percentage-string c))			       
		  (concatenate 'string
			       "C "
			       (let ((val (get-time-string c)))
				 (if val
				     (concatenate 'string
						  "- "
						  val
						  " ")
				     ""))	  
			       (get-percentage-string c)))))
      d)))

(load-module "swm-gaps")

;; Inner gaps run along all the 4 borders of a frame

;; Outer gaps add more padding to the outermost borders
;; (touching the screen border)
(setf swm-gaps:*inner-gaps-size* 10)
(setf swm-gaps:*outer-gaps-size* 0)
(run-commands "toggle-gaps")

(defcommand yay () ()
  (message "[power: ~a]" (battery-status)))
;
    ;(message "~a~%~a" "nop" "nio")
    ;(if (consp a)
    ;  (message "~%rarara   ~a" (run-shell-command (car a) t))))
					;(run-shell-command (car a))
;	(message "~a" "nop\nnop"))))



(defun transparent-xwin (xwin)
  (xlib:change-property xwin
                        :_NET_WM_WINDOW_OPACITY
                        '(4080218880)
                        ;; '(3865470464)
                        :CARDINAL
                        32))

(defun make-window-transparent (window)
  (transparent-xwin (window-parent window)))

(add-hook *new-window-hook* 'make-window-transparent)

;;; make all ui elements transparent
(dolist (s *screen-list*)
  (transparent-xwin (screen-message-window s))
  (transparent-xwin (screen-input-window s))
  (transparent-xwin (screen-key-window s))
  (transparent-xwin (screen-focus-window s))
  (transparent-xwin (screen-frame-window s))
  (dolist (h (screen-heads s))
    (when (not (null (head-mode-line h)))
      (transparent-xwin (mode-line-window (head-mode-line h))))))

;; (run-shell-command "cairo-compmgr")

(defcommand make-current-window-transparent () ()
  (make-window-transparent (current-window)))


(setf *screen-mode-line-format*
      (list "%d     [ %n ]     Net: %I | Power: "
	    `(:eval (battery-status))))
