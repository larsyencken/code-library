<html>
<head>
    <meta charset="UTF-8">
    <title>◊select['h1 doc]</title>
    <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊->html[doc]

◊(define prev-page (previous here))
◊(define next-page (next here))

The current page is called ◊|here|.
◊when/splice[prev-page]{The previous is <a href="◊|prev-page|">◊|prev-page|</a>.}
◊when/splice[next-page]{The next is <a href="◊|next-page|">◊|next-page|</a>.}
</body>

</html>
