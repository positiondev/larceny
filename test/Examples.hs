{-# LANGUAGE OverloadedStrings #-}

module Examples where

import qualified Data.Map       as M
import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import           Larceny

tpl1 :: Text
tpl1 = "<h1>Hello</h1>"

tpl2 :: Text
tpl2 = "<h1><name/></h1>"

tpl3 :: Text
tpl3 = "<apply template=\"skater\" />"

tpl4 :: Text
tpl4 = "<body>                     \
\          <bind tag=\"sport\">    \
\            Roller Derby          \
\          </bind>                 \
\          <h1>                    \
\            <name/> <sport/>      \
\          </h1>                   \
\          <ul>                    \
\            <skaters>             \
\              <li>                \
\                <h2><name/></h2>  \
\                <p><position/></p>\
\              </li>               \
\            </skaters>            \
\          </ul>                   \
\        </body>"

subst :: Substitutions ()
subst = subs [ ("site-title", textFill "Gotham Girls roster")
             , ("name", textFill "Gotham Girls")
             , ("skater", fillChildrenWith $ subs [("name", textFill "Amy Roundhouse")])
             , ("skaters", mapSubs
                          (\(n,p) ->
                             (subs [("name", textFill n)
                                   ,("position", textFill p)]))
                          [ ("Bonnie Thunders", "jammer")
                          , ("Donna Matrix", "blocker")
                          , ("V-Diva", "jammer") ] )
              , ("desc", useAttrs ((a"length" %
                                    a"L.textFill")
                                   (\n d -> textFill $ T.take n d <> "...")))
              , ("clients", clientFill) ]

tplLib :: Library ()
tplLib = M.fromList [(["skater"], parse "Beyonslay")]

tpl5 :: Text
tpl5 = "<desc length=\"10\" text=\"A really long description\" />"

tpl6 :: Text
tpl6 = "<!doctype html>\n\
        \\n\
        \<html lang=\"en\">\n\
        \  <head>\n\
        \    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n\
        \    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
        \\n\
        \    <title>position development</title>\n\
        \\n\
        \    <script src=\"//use.typekit.net/lfb8twu.js\"></script>\n\
        \    <script>try{Typekit.load();}catch(e){}</script>\n\
        \\n\
        \\n\
        \    <link rel=\"stylesheet\" href=\"css/normalize.css\" type=\"text/css\" charset=\"utf-8\">\n\
        \    <link rel=\"stylesheet\" href=\"css/skeleton.css\" type=\"text/css\" charset=\"utf-8\">\n\
        \    <link rel=\"stylesheet\" href=\"css/style.css\" type=\"text/css\" charset=\"utf-8\">\n\
        \\n\
        \  </head>\n\
        \\n\
        \\n\
        \  <body>\n\
        \\n\
        \    <apply template=\"header\" /> \
        \\n\
        \    <hr/>\n\
        \\n\
        \    <div class=\"container\">\n\
        \      <div class=\"row section\">\n\
        \        <div class=\"three columns\">\n\
        \          <h3>Our Mission</h3>\n\
        \        </div>\n\
        \        <div class=\"nine columns\">\n\
        \          <p>Position Development is a worker-run software development company, with a focus on independent media, cultural institutions and socially minded organizations.\n\
        \          </p>\n\
        \\n\
        \          <p>Our core business idea is that by organizing a team\n\
        \            specialized in the needs of independent/radical media,\n\
        \            and by being worker run, we can serve the software needs\n\
        \            of these organizations that we care about better and\n\
        \            cheaper than anyone else. We already have some great\n\
        \            clients and it seems to be working.</p>\n\
        \        </div>\n\
        \      </div> <!-- .section -->\n\
        \\n\
        \      <apply template=\"clients\" /> \
        \\n\
        \      <div class=\"row section\">\n\
        \        <div class=\"three columns\">\n\
        \          <h3>Our Services</h3>\n\
        \        </div>\n\
        \        <div class=\"nine columns\">\n\
        \          <p>We carefully pair high-end technology services to the\n\
        \            requirements and reality of our clients' day to day\n\
        \            operation. We study their needs and create unique systems\n\
        \            and processes to make sure their objectives are met.\n\
        \          </p>\n\
        \          <div class=\"service\">\n\
        \            <span class=\"icon icon-circle-compass\"></span>\n\
        \            <h4>Front and Backend Development</h4>\n\
        \          </div> <!-- .service -->\n\
        \          <div class=\"service\">\n\
        \            <span class=\"icon icon-dial\"></span>\n\
        \            <h4>Automated Testing and Deployment</h4>\n\
        \          </div> <!-- .service -->\n\
        \          <div class=\"service\">\n\
        \            <span class=\"icon icon-basket\"></span>\n\
        \            <h4>Ecommerce and Fulfillment</h4>\n\
        \          </div> <!-- .service -->\n\
        \          <div class=\"service\">\n\
        \            <span class=\"icon icon-gears\"></span>\n\
        \            <h4>Legacy Applications</h4>\n\
        \          </div> <!-- .service -->\n\
        \          <div class=\"service\">\n\
        \            <span class=\"icon icon-linegraph\"></span>\n\
        \            <h4>Technical Strategy</h4>\n\
        \          </div> <!-- .service -->\n\
        \        </div>\n\
        \      </div> <!-- .section -->\n\
        \\n\
        \\n\
        \      <div class=\"row section\">\n\
        \        <div class=\"three columns\">\n\
        \          <h3>Our Tech</h3>\n\
        \        </div>\n\
        \        <div class=\"nine columns\">\n\
        \          <div class=\"technology\">\n\
        \            <h5><span class=\"icon icon-mobile\"></span> Languages</h5>\n\
        \            <p>Ruby, Haskell, SQL, Javascript</p>\n\
        \          </div><!-- .technology -->\n\
        \          <div class=\"technology\">\n\
        \            <h5><span class=\"icon icon-layers\"></span> Databases</h5>\n\
        \            <p>Postgresql, MySQL</p>\n\
        \          </div><!-- .technology -->\n\
        \          <div class=\"technology\">\n\
        \            <h5><span class=\"icon icon-refresh\"></span> Services</h5>\n\
        \            <p>AWS, Heroku, Stripe</p>\n\
        \          </div><!-- .technology -->\n\
        \        </div>\n\
        \      </div> <!-- .section -->\n\
        \\n\
        \\n\
        \      <div class=\"row section\">\n\
        \        <div class=\"three columns\">\n\
        \          <h3>Our Team</h3>\n\
        \        </div>\n\
        \        <div class=\"nine columns\">\n\
        \          <p>We love to write excellent software and work hard with\n\
        \          our customers to assess their needs and business\n\
        \          objectives. Our team has extensive experience building\n\
        \          database backed web applications. We understand technology\n\
        \          all the way from CSS/Javascript frontend work, to Haskell\n\
        \          and Ruby backends, to deployment automation and continuous\n\
        \          integration.</p>\n\
        \\n\
        \          <p><strong>We're looking for people to join us!</strong> You can check out our job listings <a href=\"/work.html\">here.</a></p>\n\
        \        </div>\n\
        \      </div> <!-- .section -->\n\
        \    </div> <!-- .container -->\n\
        \\n\
        \    <hr/>\n\
        \\n\
        \    <apply template=\"footer\" />\
        \\n\
        \    <script>\n\
        \      (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\n\
        \      (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\n\
        \      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\n\
        \      })(window,document,'script','//www.google-analytics.com/analytics.js','ga');\n\
        \\n\
        \      ga('create', 'UA-58668184-1', 'auto');\n\
        \      ga('require', 'linkid', 'linkid.js');\n\
        \      ga('send', 'pageview');\n\
        \    </script>\n\
        \  <script type=\"text/javascript\">/* <![CDATA[ */(function(d,s,a,i,j,r,l,m,t){try{l=d.getElementsByTagName('a');t=d.createElement('textarea');for(i=0;l.length-i;i++){try{a=l[i].href;s=a.indexOf('/cdn-cgi/l/email-protection');m=a.length;if(a&&s>-1&&m>28){j=28+s;s='';if(j<m){r='0x'+a.substr(j,2)|0;for(j+=2;j<m&&a.charAt(j)!='X';j+=2)s+='%'+('0'+('0x'+a.substr(j,2)^r).toString(16)).slice(-2);j++;s=decodeURIComponent(s)+a.substr(j,m-j)}t.innerHTML=s.replace(/</g,'&lt;').replace(/>/g,'&gt;');l[i].href='mailto:'+t.value}}catch(e){}}}catch(e){}})(document);/* ]]> */</script></body>\n\
        \\n\
        \</html>"

data Client = Client { clientName :: Text
                     , clientUrl  :: Text
                     , clientLogo :: Text } deriving (Eq, Show)

clientFill :: Fill ()
clientFill = mapSubs (\(Client name url logo) ->
                          subs [ ("client-name", textFill name)
                               , ("client-url", textFill url)
                               , ("client-logo", textFill logo)]) clients

clients :: [Client]
clients = [Client "Seven Stories Press"
                  "http://www.sevenstories.com"
                  "img/logo_ssp.gif",
           Client "Real Food Challenge"
                  "http://calculator.realfoodchallenge.org"
                  "img/logo_realfood.gif",
           Client "Haymarket Books"
                  "http://www.haymarketbooks.org"
                  "img/logo_haymarket.gif",
           Client "Verso Books"
                  "http://www.versobooks.com"
                  "img/logo_verso.gif",
           Client "Jacobin Magazine"
                  "http://www.jacobinmag.com"
                  "img/logo_jacobin.gif"]

clientsTpl :: LT.Text
clientsTpl =
        "<div class=\"row section\">\n                           \
        \  <div class=\"three columns\">\n                       \
        \    <h3>Our Clients</h3>\n                              \
        \  </div>\n                                              \
        \  <div class=\"nine columns clients\">\n                \
        \    <clients>                                           \
        \      <div class=\"client\">\n                          \
        \        <div class=\"logo\">\n                          \
        \          <a href=\"${client-url}\" target=\"_blank\">\n\
        \            <img title=\"${client-name\"                \
        \                 src=\"${client-logo}\"/>\n             \
        \          </a>\n                                        \
        \        </div>\n                                        \
        \        <a href=\"${client-url}\" target=\"_blank\">\n  \
        \          <client-name />\n                             \
        \        </a>\n                                          \
        \      </div> <!-- .client -->\n                         \
        \    </clients>                                          \
        \  </div>\n                                              \
        \</div> <!-- .section -->\n"

headerTpl :: LT.Text
headerTpl =
        "  <div class=\"container\">\n\
        \      <div class=\"row\">\n\
        \        <div class=\"five columns\">\n\
        \          <h1><a href=\"/\">position development</a></h1>\n\
        \        </div>\n\
        \        <div class=\"three columns\">\n\
        \          &nbsp;\n\
        \        </div>\n\
        \        <div class=\"four columns\">\n\
        \          <p class=\"tagline\">Want to hire us? <a href=\"/cdn-cgi/l/email-protection#94e3fbe6fff1e6e7d4e4fbe7fde0fdfbfaf0f1e2baf7fbf9\"><strong><span class=\"__cf_email__\" data-cfemail=\"ed9a829f86889f9ead9d829e849984828389889bc38e8280\">[email&#160;protected]</span><script data-cfhash='f9e31' type=\"text/javascript\">/* <![CDATA[ */!function(t,e,r,n,c,a,p){try{t=document.currentScript||function(){for(t=document.getElementsByTagName('script'),e=t.length;e--;)if(t[e].getAttribute('data-cfhash'))return t[e]}();if(t&&(c=t.previousSibling)){p=t.parentNode;if(a=c.getAttribute('data-cfemail')){for(e='',r='0x'+a.substr(0,2)|0,n=2;a.length-n;n+=2)e+='%'+('0'+('0x'+a.substr(n,2)^r).toString(16)).slice(-2);p.replaceChild(document.createTextNode(decodeURIComponent(e)),c)}p.removeChild(t)}}catch(u){}}()/* ]]> */</script></strong></a></p>\n\
        \\n\
        \          <p>Want to <a href=\"/work.html\"><strong>join us</strong></a>?</p>\n\
        \        </div>\n\
        \      </div> <!-- .row -->\n\
        \    </div> <!-- .container -->\n"

footerTpl :: LT.Text
footerTpl =
        "    <div class=\"container footer\">\n\
        \      <div class=\"row\">\n\
        \        <div class=\"twelve columns\">\n\
        \          <a href=\"/index.html\">Home</a> | \n\
        \          <a href=\"/work.html\">Jobs</a> &mdash; \n\
        \          &copy; 2016 Position Development, LLC. \n\
        \        </div>\n\
        \      </div> <!-- .row -->\n\
        \    </div> <!-- .container -->\n"

positionTplLib :: Library ()
positionTplLib = M.fromList
  [ (["header"],  parse headerTpl)
  , (["clients"], parse clientsTpl)
  , (["footer"],  parse footerTpl) ]
