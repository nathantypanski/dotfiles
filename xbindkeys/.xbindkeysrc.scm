;; (remove-xbindkey key)
;; (run-command "foo-bar-command [args]")
(grab-all-keys)
;; (ungrab-all-keys)
;; (remove-all-keys)
(debug)

;; Toggle the touchpad
;; XF86TouchpadToggle
(xbindkey '("m:0x0" "c:199") "synclient TouchpadOff=`synclient -l | grep -ce TouchpadOff.*0`")

;; increase volume
;; XF86AudioRaiseVolume
(xbindkey '("m:0x0" "c:123") "pamixer --increase 5")

;; decrase volume
;; XF86AudioLowerVolume
(xbindkey '("m:0x0" "c:122") "pamixer --decrease 5")

;; (xbindkey '("m:0x0" "c:114") "xsel | xvkbd -xsendevent -file -")

(xbindkey '("m:0x0" "c:233")
          "sudo bright-up")

(xbindkey '("m:0x0" "c:232")
          "sudo bright-down")

;; Toggle mute
;; XF86AudioMute
(xbindkey '("m:0x0" "c:121") "pamixer --toggle-mute")

;;(xbindkey '(Mod4 Return) "urxvt")
(xbindkey '(Mod4 p) "dmenu_run")

(xbindkey '(Mod4 Return) "urxvt")

;; (xbindkey '(Mod4 Shift grave) "~/bin/emc")

