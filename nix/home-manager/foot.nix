{ termFont, ... }:

{
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = termFont;
      };
      colors = {
        foreground = "dcdccc";
        background = "121212";
        selection-foreground = "3f3f3f";
        selection-background = "dcdccc";

        # ANSI 0–7
        regular0  = "3f3f3f";  # black
        regular1  = "705050";  # red
        regular2  = "60b48a";  # green
        regular3  = "dfaf8f";  # yellow
        regular4  = "506070";  # blue
        regular5  = "dc8cc3";  # magenta
        regular6  = "8cd0d3";  # cyan
        regular7  = "dcdccc";  # white (light gray)
        # Bright 8–15
        bright0   = "709080";  # bright black (gray)
        bright1   = "dca3a3";  # bright red
        bright2   = "c3bf9f";  # bright green
        bright3   = "f0dfaf";  # bright yellow
        bright4   = "94bff3";  # bright blue
        bright5   = "ec93d3";  # bright magenta
        bright6   = "93e0e3";  # bright cyan
        bright7   = "ffffff";  # bright white
      };
    };
  };
}
