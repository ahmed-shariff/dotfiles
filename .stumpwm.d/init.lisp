(in-package :stumpwm)
(defparameter *system-name* "msi"); "thinkpad" "msi"
(set-prefix-key (kbd "C-z"))

(load "/home/amsha/quicklisp/setup.lisp")
;; (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil) ;; only once

(ql:quickload '(:cl-utilities :clx-truetype :xembed))
;		:cl-emoji))

;; change the prefix key to something else

(set-module-dir "~/.stumpwm.d/modules")

(run-shell-command "feh --randomize --bg-fill $HOME/Documents/Pictures/Background*")
(run-shell-command "compton &")
;; (run-shell-command "conky -c .conky/custom/conky.conf &")
(run-shell-command "tint2")
(run-shell-command "xbingkeys")
(run-shell-command "xset dpms 3600 3600 3600")
(run-shell-command "nm-applet")

(define-key *root-map* (kbd "c") "exec tilix --window-style=disable-csd-hide-toolbar");xfce4-terminal --hide-menubar")
(define-key *root-map* (kbd "e") "exec (unset SBCL_HOME;emacs &)")
(define-key *root-map* (kbd "w") "frame-windowlist")
(define-key *root-map* (kbd ".") "mymenu")
(define-key *root-map* (kbd "C-s") "exec emacsclient -a '' -e '(org-capture)'")
(define-key *root-map* (kbd "C-o") "gother")
(define-key *root-map* (kbd "C-TAB") "call-function-and-frame-windowlist (next)")
(define-key *root-map* (kbd "C-M-TAB") "call-function-and-frame-windowlist (prev)")

(defcommand call-function-and-frame-windowlist (&optional body) (:rest)
  (when body
    (eval-line body))
  (let* ((group (current-group))
	 (frame (tile-group-current-frame group)))
    (echo-windows *window-format* (current-group) (frame-windows group frame))))

(defcommand reboot () ()
  (run-shell-command "reboot"))
(defcommand shutdown () ()
  (run-shell-command "shutdown -h now"))

(defvar *quake-window* nil)
(defun toggle-quake-window ()
  (if (equalp *quake-window* nil)
      (progn
	)
      (pull-window *quake-window*)))

(defcommand toggle () ()
  (toggle-quake-window));;;restart-hard

;; (run-commands "mode-line")
(load-module "stumptray")
(load-module "wifi")
(load-module "mem")
(load-module "net")
(load-module "cpu")
(load-module "battery-portable")
(load-module "ttf-fonts")
;; (stumptray::stumptray)
(setf *message-window-padding* 150)
(setf *message-window-gravity* :top)
(setf *input-window-gravity* :top)


(set-fg-color "#ffad29")
(set-bg-color "#253142")
;;(set-font "-misc-dejavu sans-medium-r-normal--0-0-0-0-p-0-iso8859-11")
(xft:cache-fonts)
(set-font (list (make-instance 'xft:font
                               :family "DejaVu Sans"
			       :subfamily "Book"
                               :size 10)))

(set-border-color "#1f3459")
(set-msg-border-width 3)

(defvar *power-status* "upower -i $(upower -e | grep BAT) |grep -E \"state|to\ empty|to\ full|percentage\"")

(defparameter *app-menu* `(("Opera" "opera")
			   ("Media"
					;sub-menu
			    ("Music" "vlc ~/Music")
			    ("Open media folder" "thunar ~/Documents/Media")
			    ("VLC player" "vlc"))
			   ("File Manager" "thunar")
			   ("Check power status" t (*power-status* t)) ;; "upower -i $(upower -e | grep BAT) |grep -E \"state|to\ empty|to\ full|percentage\""
			   ("Screen settings"
			    ("Change background" "feh --randomize --bg-fill ~/Documents/Pictures/Background/*")
			    ;; ("Lock screen" "xset dpms force off;~/./.stumpwm.d/betterlockscreen/betterlockscreen -l dim")
			    ("Lock screen" "~/./.stumpwm.d/betterlockscreen/betterlockscreen --off 3600 -t 'wink\! wink\!' -l dim && ~/./.stumpwm.d/betterlockscreen/betterlockscreen -u ~/Documents/Pictures/Background/")
			    ("Brightness -" ,(lambda ()
					       (concatenate 'string
							    "light "
							    (if (< (read-from-string (run-shell-command "light -G" t)) 20)
								"-S 20"
								"-U 12"))))
			    ("Brightness +" "light -A 12")
			    ("Brightness set" ,(lambda ()
						 (concatenate 'string
							      "light -S "
							      (read-one-line (current-screen) "Brightness Value: ")))))))

(defcommand mymenu () ()
  (labels ((pick (options)
	     (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
;	       (echo (funcall (second selection))))))
	       (cond
		 ((null selection)
		  (throw 'stumpwm::error "Abort."))
		 ((equalp (type-of (second selection)) 'function)
		  (funcall (second selection)))
		 ((consp (cadr selection))
		  (pick (cdr selection)))
		 ((stringp (second selection))
		  (second selection))
		 (t
		  (cddr selection))))))
    (let ((choice (pick *app-menu*)))
      (if (consp choice)
	  (run-shell-command (caar choice) (cdar choice));(run-shell-command (car a))
	  (run-shell-command choice)))))
							
(defun shell-command-fn (command) "Run a shell command and display
    output to screen.  This must be used in a functional
    side-effects-free style! If a program does not exit of its own
    accord, Stumpwm might hang!"
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
;; Removing setting gaps to resolve buf with stump crashing
;; (setf swm-gaps:*inner-gaps-size* 10)
;; (setf swm-gaps:*outer-gaps-size* 0)
(run-commands "toggle-gaps")

(defcommand yay () ()
  (message "[power: ~a]" (battery-status)))
;
    ;(message "~a~%~a" "nop" "nio")
    ;(if (consp a)
    ;  (message "~%rarara   ~a" (run-shell-command (car a) t))))
					;(run-shell-command (car a))
;	(message "~a" "nop\nnop"))))



;; (defun transparent-xwin (xwin)
;;   (xlib:change-property xwin
;;                         :_NET_WM_WINDOW_OPACITY
;;                         '(4080218880)
;;                         ;; '(3865470464)
;;                         :CARDINAL
;;                         32))

;; (defun make-window-transparent (window)
;;   (transparent-xwin (window-parent window)))

;; (add-hook *new-window-hook* 'make-window-transparent)

;; ;;; make all ui elements transparent
;; (dolist (s *screen-list*)
;;   (transparent-xwin (screen-message-window s))
;;   (transparent-xwin (screen-input-window s))
;;   (transparent-xwin (screen-key-window s))
;;   (transparent-xwin (screen-focus-window s))
;;   (transparent-xwin (screen-frame-window s))
;;   (dolist (h (screen-heads s))
;;     (when (not (null (head-mode-line h)))
;;       (transparent-xwin (mode-line-window (head-mode-line h))))))

;; (run-shell-command "cairo-compmgr")

(defcommand make-current-window-transparent () ()
  (make-window-transparent (current-window)))

(setf cpu::*cpu-modeline-fmt* "%c")

(setf *screen-mode-line-format*
      (list (format nil "^(:fg \"white\")^[^(:bg \"#DD0000\") %d    [%n]    ^]^[^(:bg \"#3333AA\") Net: %I %l ^]^[^(:fg \"#339900\") Power: %B ^]|^[^(:fg \"#770077\") %C ^]|^[^(:fg \"#009999\") %M ^]")))
;;`(:eval (battery-status)))))
					;(list "%d     [^[^(:bg \"#555511\") %n ^]]     Net: %I | Power: "	    
					;	    ))

(defcommand gnew-train () ()
  (add-group (current-screen) "train")
  (if (equalp *system-name* "Thinkpad")
      (progn
	(restore-from-file "train-layout-thinkpad")
	(restore-window-placement-rules "~/.stumpwm.d/settings/train-window-rules-thinkpad")
	;(run-shell-command "tilix --title=0 --window-style=disable-csd-hide-toolbar")
	;(run-shell-command "xfce4-terminal --hide-menubar --initial-title 0 -x htop")
	(run-shell-command "xfce4-terminal --hide-menubar --initial-title 1 -x htop"))
      (progn 
	(restore-from-file "train-layout-msi")
	(restore-window-placement-rules "~/.stumpwm.d/settings/train-window-rules-msi")
	(run-shell-command "xfce4-terminal --hide-menubar --zoom -1 --initial-title 1 -x htop")
	(run-shell-command "xfce4-terminal --hide-menubar --zoom -1 --initial-title 2 -x watch -n 0.3 nvidia-smi")
	(run-shell-command "xfce4-terminal --hide-menubar --zoom -1 --initial-title 4 -x journalctl -f ")
	(run-shell-command "xfce4-terminal --hide-menubar --zoom -1 --initial-title 3 -x watch -n 0.3 sensors")
	;;(run-shell-command "tilix --window-style=disable-csd-hide-toolbar -t 0"))))
        )))
