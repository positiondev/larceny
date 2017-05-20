<!DOCTYPE html>

<html>
  <head>
    <title>
      <page-title />
    </title>
    <link rel="stylesheet" href="/minimal.css" />
    <style>
     header { padding: 1em; }
     li { list-style: none }
     #main li {
       border: 1px solid black;
       padding: 1em 0 0 1em;
       margin-bottom: 1em;
     }
     #main {
       margin: 0 0 1em 1em;
       width: 40em;
     }
     footer { padding: 1em; }
    </style>
  </head>

  <body>
    <apply template="header" />
    <div id="main">
      <apply-content />
    </div>
    <apply template="footer" />
  </body>
</html>
