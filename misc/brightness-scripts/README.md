# brightness scripts

I install these at `/usr/local/bin/bright-up` and `/usr/local/bin/bright-down`
in order to control the brightness on my laptop. Then I add the following to
`/etc/sudoers`:

```
Cmnd_Alias     BRIGHTNESS = /usr/local/bin/bright-up, /usr/local/bin/bright-down
nathan ALL=(ALL) NOPASSWD: BRIGHTNESS
```

to make them runnable by my regular user. Using
[`xbindkeysrc.scm`](../../xbindkeys/.xbindkeysrc.scm)
I bind the brightness keys on my laptop to those scripts.
