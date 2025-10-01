{ config, userEmail, username, pkgs, lib, ... }:

let
  name  = "ndt";
  gmail = "you@gmail.com";
  fast  = "you@fastmail.com";
in

let
  # ---- 1) single source of truth: localpart only ----
  name            = "ndt";
  gmailDomain     = "gmail.com";
  fastmailDomain  = "fastmail.com";
  gmail           = "${name}@${gmailDomain}";
  fast            = "${name}@${fastmailDomain}";

  mailRoot        = "${config.xdg.dataHome}/mail";
  gmailDir        = "${mailRoot}/gmail";
  fastDir         = "${mailRoot}/fastmail";

  # passage prints fields; we want just the first line (password)
  mailSecret = "${config.home.homeDirectory}/.local/bin/mail-secret";

  # small helper to build an accounts.email record (reduces duplication)
  mkAccount = {
    id, addr, realName, imapHost, smtpHost, dir, drafts, sent, trash, archive,
    pwPath, spamPattern
  }: {
    "${id}" = {
      # HM will generate neomutt/msmtp/mbsync configs from this.
      address   = addr;
      realName  = realName;
      userName  = addr;

      imap = {
        host = imapHost;
        port = 993;
        tls.enable = true;
        # Prefer implicit TLS on 993 (no STARTTLS downgrade dance)
        tls.useStartTls = false;
      };
      smtp = {
        host = smtpHost;
        port = 465;
        tls.enable = true;
        # Prefer implicit TLS on 465
        tls.useStartTls = false;
      };

      # 2) passage-based secret retrieval
      passwordCommand = "${mailSecret} ${pwPath}";

      mailbox = {
        path = dir;
        folders = {
          inbox   = "INBOX";
          drafts  = drafts;
          sent    = sent;
          trash   = trash;
          archive = archive;
        };
      };

      # Sync + excludes
      mbsync = {
        enable = true;
        create = "both";
        patterns = [ "*" spamPattern ];
        extraConfig.channel = ''
          SyncState *
          PipelineDepth 1
        '';
      };

      msmtp.enable    = true;
      notmuch.enable  = true;
      neomutt.enable  = true;
    };
  };
in
{
  # Core tooling
  programs.neomutt.enable = true;
  programs.notmuch.enable = true;
  programs.msmtp.enable   = true;

  # mbsync cron via HM option (every 5 minutes)
  services.mbsync = {
    enable = true;
    frequency = "*:0/5";
    verbose = true;
  };

  # Tools: notmuch helpers, HTML renderer, passage (age-backed), etc.
  home.packages = with pkgs; [
    notmuch-mutt
    w3m
    passage
    urlscan ripmime wl-clipboard
  ];

  # 2) passage helper (first line only, like classic pass)
  home.file.".local/bin/mail-secret" = {
    mode = "0755";
    text = ''
      #!/usr/bin/env bash
      set -euo pipefail
      entry="${1:?usage: mail-secret path/in/store}"
      ${pkgs.passage}/bin/passage show "$entry" | head -n1
    '';
  };

  # Make sure maildirs exist
  home.activation.ensureMailDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p "${gmailDir}" "${fastDir}"
  '';

  # 3) Declarative accounts via our helper
  accounts.email.accounts =
    (mkAccount {
      id = "gmail";
      addr = gmail;
      realName = name;
      imapHost = "imap.gmail.com";
      smtpHost = "smtp.gmail.com";
      dir = gmailDir;
      drafts = "[Gmail]/Drafts";
      sent = "[Gmail]/Sent Mail";
      trash = "[Gmail]/Trash";
      archive = "[Gmail]/All Mail";
      pwPath = "email/gmail/app";         # <— create with passage insert email/gmail/app
      spamPattern = "![Gmail]/Spam";
    }) //
    (mkAccount {
      id = "fastmail";
      addr = fast;
      realName = name;
      imapHost = "imap.fastmail.com";
      smtpHost = "smtp.fastmail.com";
      dir = fastDir;
      drafts = "Drafts";
      sent   = "Sent";
      trash  = "Trash";
      archive = "Archive";
      pwPath = "email/fastmail/app";      # <— create with passage insert email/fastmail/app
      spamPattern = "!Spam";
    });

  # 4) NeoMutt customizations (vi keys + notmuch)
  programs.neomutt.extraConfig = ''
    set edit_headers=yes
    set mbox_type=Maildir
    set folder="${mailRoot}"
    set spoolfile="${gmailDir}/INBOX"
    set record="+gmail/[Gmail]/Sent Mail"
    set postponed="+gmail/[Gmail]/Drafts"

    set header_cache="${config.xdg.cacheHome}/neomutt/headers"
    set message_cachedir="${config.xdg.cacheHome}/neomutt/bodies"
    set tmpdir="${config.xdg.runtimeDir:-/tmp}/neomutt-tmp"

    # TLS-only; disallow opportunistic downgrade
    set ssl_starttls=yes
    set ssl_force_tls=yes

    set sort=threads
    set sort_aux=last-date-received
    set pager_index_lines=10
    set editor="vim"
    set thorough_search=yes
    auto_view text/html
    set mailcap_path="${config.xdg.configHome}/mailcap"

    # notmuch integration
    macro index \es "<shell-escape>notmuch new<enter><refresh>"

    # Vim-like bindings
    bind index j next-entry
    bind index k previous-entry
    bind index gg first-entry
    bind index G  last-entry
    bind index dd delete-message
    bind index u  undelete-message
    bind index /  search
    bind index n  search-next
    bind index N  search-opposite
    bind pager j next-line
    bind pager k previous-line
    bind pager gg top
    bind pager G  bottom

    # Explicit account switchers
    macro index ,g "<enter-command>set from=${gmail}; my_hdr From: ${name} <${gmail}>; set spoolfile=${gmailDir}/INBOX; set record=+gmail/[Gmail]/Sent Mail; set postponed=+gmail/[Gmail]/Drafts<enter>"
    macro index ,f "<enter-command>set from=${fast};  my_hdr From: ${name} <${fast}>;  set spoolfile=${fastDir}/INBOX;  set record=+fastmail/Sent;            set postponed=+fastmail/Drafts<enter>"
  '';

  # Safe HTML renderer
  xdg.configFile."mailcap".text = ''
    text/html; ${pkgs.w3m}/bin/w3m -dump -T text/html -o display_link_number=1 -o decode_url=1 -I %{charset} -o confirm_qq=0 %s; \
      nametemplate=%s.html; copiousoutput
  '';

  # notmuch DB root at the parent of both maildirs
  programs.notmuch.extraConfig = {
    database.path = mailRoot;
    user = {
      name  = name;
      primary_email = gmail;
      other_email   = [ fast ];
    };
  };
}
