{
  language_server-protocol = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1k0311vah76kg5m6zr7wmkwyk5p2f9d9hyckjpn3xgr83ajkj7px";
      type = "gem";
    };
    version = "3.17.0.5";
  };
  logger = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "00q2zznygpbls8asz5knjvvj2brr3ghmqxgr83xnrdj4rk3xwvhr";
      type = "gem";
    };
    version = "1.7.0";
  };
  prism = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0d3i31npmlhmigcs1zlb9lksg7z7lv6nffams71wrz5rriv1n35l";
      type = "gem";
    };
    version = "1.5.1";
  };
  rbs = {
    dependencies = ["logger"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1c0r26dhdr4jiklc0g7wjmr5q56dp7hwcfa8z75khkp8mrhazfpa";
      type = "gem";
    };
    version = "3.9.5";
  };
  ruby-lsp = {
    dependencies = ["language_server-protocol" "prism" "rbs"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "050qg73g4qs3xza441nafy7610daa3k4ra0pbi3sdlawy9fwfh6i";
      type = "gem";
    };
    version = "0.26.1";
  };
  sorbet-runtime = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0d15x827rqjg60bigl4nc78zacg6hf1g0n2gac9dw1437j2xsls1";
      type = "gem";
    };
    version = "0.6.12586";
  };
}
