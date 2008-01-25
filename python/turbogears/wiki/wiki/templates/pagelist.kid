<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:py="http://purl.org/kid/ns#"
    py:extends="'master.kid'">
<head>
<meta content="text/html; charset=utf-8" http-equiv="Content-Type" py:replace="''"/>
<title>Page Listing</title>
</head>
<body>
  <div class="main_content">
    <h1>All of your pages</h1>
    <ul>
      <li py:for="pagename in pages">
        <a href="${tg.url('/' + pagename)}"
            py:content="pagename">Page name goes here.</a>
      </li>
    </ul>
  </div>
</body>
</html>
