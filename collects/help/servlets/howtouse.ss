(module howtouse mzscheme
  (require (lib "servlet-helpers.ss" "web-server"))
  
  (require "private/util.ss")
  (require "private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (TITLE "Help Desk")
      (HEAD ,hd-css
            ,@hd-links)
      (BODY 
       (H1 "Help Desk") 
       (P)
       (A ((NAME "helpme") (VALUE "Help Desk")))
       "Help Desk (the program you're currently running) is a "
       "complete source of information about PLT software, "
       "including DrScheme, MzScheme, and MrEd."
       (P)
       "Use Help Desk to find information in either of two ways:"
       (P)
       ,(color-highlight
         "1) Navigate the Help Desk information pages by "
         "clicking on hyperlinks.")
       (UL 
        (LI  "The " (B  (TT  "Help Desk home")) " link "
             "at the top of the page always takes "
             "you back to the starting page.")
        (LI  "The " (B  (TT  "Show manuals")) " link "
             " displays a list "
             " of manuals and other documentation.")
        (LI  "The " (B  (TT  "Send bug report")) " and "
             (B (TT  "Query bug reports")) " links "
             "allow you to submit bug reports to PLT and track their "
             "status."))
       (P)
       (A ((NAME "helpsearch") (VALUE "Searching in Help Desk")))
       (A ((NAME "search")))
       ,(color-highlight
         "2) Search for terms using the " 
         `(B  (TT  "Search for"))  
         " field at the top of Help Desk.")
       (UL  
        (LI  "Enter one or more terms into the " 
             (B  (TT  "Search for")) " field.")
        (LI  "Click the " (B  "Search") " button "
             "(or hit Enter) to start a search, "
             "or click on the " (B "Lucky!") " button.")
        (LI  "If you click on the " (B "Search") " button, "
             "Help Desk scans the documentation pages and "
             "returns a list of hyperlinks for "
             (I  "keyword") ", "
             (I  "index entry") ", and "
             (I  "raw text")  " matches:")
        (UL 
         (LI  (I  "Keywords") " are Scheme names, "
              "such as " (TT  "define") " and "
              (TT  "cons") ".") 
         (LI  (I  "Index entries")
              " are topical phrases, such as \"lists\".")
         (LI  (I  "Raw text") " results are fragments of "
              "text from the documentation pages. "
              "(Raw text results are useful only as "
              "a last resort.)"))
        (LI "If you click on the " (B "Lucky!") " button, "
            "Help Desk displays only the first item of documentation "
            "that matches the search term, without displaying links to "
            "all relevant items.")
        (LI  "Expert users can adjust the search with the "
             "popdown menus below the "
             (B (TT "Search for")) " field."))
       (P)
       "Help Desk sorts search results according to their source, "
       "and then according to their (apparent) relevance."))))