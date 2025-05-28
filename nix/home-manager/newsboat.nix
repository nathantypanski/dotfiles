{ browser, ... }:

{
  programs.newsboat = {
    enable = true;
    browser = browser;
    maxItems = 50;
    urls = [
      { url = "https://words.filippo.io/rss/"; }
      { url = "https://linderud.dev/blog/index.xml"; }
      { url = "https://krebsonsecurity.com/feed/atom"; }
      { url = "http://export.arxiv.org/api/query?search_query=cat:cs.CR&sortBy=submittedDate&sortOrder=descending&max_results=50"; }
      { url = "http://seclists.org/rss/fulldisclosure.rss"; }
      { url = "http://feeds.feedburner.com/Unit42"; }
      { url = "https://nedwill.github.io/blog/feed.xml"; }
      { url = "https://nathantypanski.com/atom.xml"; }
      { url = "https://hnrss.org/frontpage"; }
    ];
  };
}
