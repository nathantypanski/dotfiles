;; Toggle the touchpad
;; XF86TouchpadToggle
(xbindkey '("m:0x0" "c:199") "synclient TouchpadOff=`synclient -l | grep -ce TouchpadOff.*0`")

;; increase volume
;; XF86AudioRaiseVolume
(xbindkey '("m:0x0" "c:123") "amixer set Master playback 1+")

;; decrase volume
;; XF86AudioLowerVolume
(xbindkey '("m:0x0" "c:122") "amixer set Master playback 1-")

;; Toggle mute
;; XF86AudioMute
(xbindkey '("m:0x0" "c:121") "amixer set Master toggle")

(xbindkey '(Mod4 Return) "urxvtc")
   
(define (define-chord-keys key1 key2 cmd-k1 cmd-k2 cmd-k1-k2 cmd-k2-k1)
    "Define chording keys"
  (let ((k1 #f) (k2 #f))
    (xbindkey-function key1 (lambda () (set! k1 #t)))
    (xbindkey-function key2 (lambda () (set! k2 #t)))
    (xbindkey-function (cons 'release key1)
		       (lambda ()
			 (if (and k1 k2)
			     (run-command cmd-k1-k2)
			     (if k1 (run-command cmd-k1)))
			 (set! k1 #f) (set! k2 #f)))
    (xbindkey-function (cons 'release key2)
		       (lambda ()
			 (if (and k1 k2)
			     (run-command cmd-k2-k1)
			     (if k2 (run-command cmd-k2)))
			 (set! k1 #f) (set! k2 #f)))))
