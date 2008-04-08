<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<?python import sitetemplate ?>
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:py="http://purl.org/kid/ns#" py:extends="sitetemplate">

<head py:match="item.tag=='{http://www.w3.org/1999/xhtml}head'" py:attrs="item.items()">
    <meta content="text/html; charset=UTF-8" http-equiv="content-type" py:replace="''"/>
    <title py:replace="''">Your title goes here</title>
    <meta py:replace="item[:]"/>
    <style type="text/css">
        #pageLogin
        {
            font-size: 10px;
            font-family: verdana;
            text-align: right;
        }
    </style>
    <style type="text/css" media="screen">
      @import "${tg.url('/static/css/style.css')}";
    </style>
    <script type="text/javascript">
      var pagesDisplayed = false;
      addLoadEvent(function() {
        connect($('pagelist'), 'onclick', function (e) {
          if (pagesDisplayed) {
            var currentPageList = UL(null, map(LI, result["pages"]));
            replaceChildNodes("pagelist_results", currentPageList);
          } else {
            e.preventDefault();
            var d = loadJSONDoc("${std.url('/pagelist', tg_format='json')}");
            d.addCallback(showPageList);
          }
          pagesDisplayed = !pagesDisplayed;
        });
      });

      function showPageList(result) {
        var currentPageList = UL(null, map(row_display, result["pages"]));
        replaceChildNodes("pagelist_results", currentPageList);
      }

      function row_display(pagename) {
        return LI(null, A({"href": "${std.url('/')}" + pagename}, pagename));
      }
    </script>

</head>

<body py:match="item.tag=='{http://www.w3.org/1999/xhtml}body'" py:attrs="item.items()">
    <div py:if="tg.config('identity.on') and not defined('logging_in')" id="pageLogin">
        <span py:if="tg.identity.anonymous">
            <a href="${tg.url('/login')}">Login</a>
        </span>
        <span py:if="not tg.identity.anonymous">
            Welcome ${tg.identity.user.display_name}.
            <a href="${tg.url('/logout')}">Logout</a>
        </span>
    </div>
    <div id="header">&nbsp;</div>
    <div id="main_content">
    <div id="status_block" class="flash" py:if="value_of('tg_flash', None)" py:content="tg_flash"></div>

    <div py:replace="[item.text]+item[:]"/>

    <!-- End of main_content -->
    </div>
<div id="footer"> <img src="${tg.url('/static/images/under_the_hood_blue.png')}" alt="TurboGears under the hood" />
  <p>View the <a id="pagelist" href="${tg.url('/pagelist')}">complete list of pages</a>.</p>
  <div id="pagelist_results"></div>
  <p>TurboGears is a open source front-to-back web development
    framework written in Python</p>
  <p>Copyright &copy; 2007 Kevin Dangoor</p>
</div>
</body>

</html>
