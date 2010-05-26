class Test {
    // change the HTML content of a DIV based on its ID
    static function setContent(id, content) {
        var d = js.Lib.document.getElementById(id);
        if (d == null) {
            js.Lib.alert("Unknown element : " + id);
        }
        d.innerHTML = content;
    }

    // create a js HTML link
    static function makeLink(title, code) {
        return '<a href="javascript:' + code + '">' + title + '</a>';
    }

    // function called when the user clicks on the link
    static function click() {
        setContent("main", "Congratulations!");
    }

    static function main() {
        // set initial content
        setContent("main", makeLink("click here", "Test.click()"));
    }
}
