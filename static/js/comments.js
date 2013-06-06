

$(document).ready(function() {
  console.log("welcome");
  $(".csubmit").click(function() {
    console.log("submit was clicked");
    var dataString = $("#commentForm").serialize();
    var pid = $("#post").val();
    var text = $("#text").val();
    var par = $("#parent").val();
    console.log("text: " + text);
    if ((text != '') && (text != null)) {
      console.log("submitting datastring: " + dataString);
      $.ajax({
        dataType: "json",
        type: "POST",
        url: "",
        data: dataString,
        success: function(data) {
          console.log("successfully submitted form");
          console.log("return value: " + data);
          console.log("type: " + (typeof data));
          var array = data;
          console.log("length: " + (array.length));
          var c1 = array[array.length - 1];
          console.log("c1 text: " + c1.text + "; text: " + text);
          var form = "<form action=\"\" id=\"commentForm\" method=\"POST\"> <input type=\"hidden\" id=\"author\" name=\"author\" value=" + author + "> <input type=\"hidden\" id=\"post\" name=\"post\" value=" + pid + "> <div> Post a reply <br> <input type=\"text\" id=\"text\" name=\"text\"></div> <input type=\"hidden\" id=\"parent\" value=" + par + "> <p><input type=\"submit\" class=\"csubmit\" id=\"submit\" value=\"Post\"</p>"
          var html = "<ul><h3>" + c1.author + "</h3>" + c1.text;
          if (par == "") {
            console.log("no parent");
            $("#root").prepend(html + form);
          } else {
            console.log("parent: " + par);
            $("#"+par).append(html + form);
          }
          return data;
        },
      });
    }
    return false;
  });
});

/*
$(document).ready(function() {
  console.log("welcome");
    $(".csubmit").click(function() {
      var element = $(this);
      //var id = element.attr("id");
      //console.log("id: " + id);

      alert("submit was clicked");
      //var dataString = $("#commentForm").serialize();
      var pid = $("#post").val();
      var text = $("#text").val();
      var par = $("#parent").val();
      var author = $("#author").val();
      alert("text: " + text);
      var dataString = "author=" + author + "&post=" + pid + "&text=" + text + "&parent=" + par;
      //var dataString = $(".csubmit").serialize();
      console.log("data: " + dataString);
      if ((text != '') && (text != null) && (text != "undefined")) {
        console.log("submitting datastring: " + dataString);
        $.ajax({
          dataType: "json",
          type: "POST",
          url: "",
          data: dataString,
          success: function(data) {
            console.log("successfully submitted form");
            console.log("return value: " + data);
            console.log("type: " + (typeof data));
            var array = data;
            console.log("length: " + (array.length));
            var c1 = array[array.length - 1];
            console.log("c1 text: " + c1.text + "; text: " + text);
            var form = "<form action=\"\" id=\"replyForm\" method=\"POST\"> <input type=\"hidden\" id=\"author\" name=\"author\" value=" + author + "> <input type=\"hidden\" id=\"post\" name=\"post\" value=" + pid + "> <div> Post a reply <br> <input type=\"text\" id=\"text\" name=\"text\"></div> <input type=\"hidden\" id=\"parent\" value=" + par + "> <p><input type=\"submit\" class=\"csubmit\" id=\"submit\" value=\"Post\"</p>"
            var html = "<ul><h3>" + author + "</h3>" + text;
            if (par == "") {
              console.log("no parent");
              $("#root").prepend(html + form);
            } else {
              console.log("parent: " + par);
              $("#"+par).append(html + form);
            }
            return data;
          },
        });
      }
      return false;
    });
});

$(document).ready(function() {
  console.log("welcome");
  //for (b in $(".submit")) {
    //$(b).click(function() {
    $("#commentForm").submit(function() {
      console.log("submit was clicked");
      var dataString = $("#commentForm").serialize();
      var pid = $("#post").val();
      var text = $("#text").val();
      var par = $("#parent").val();
      console.log("text: " + text);
      if ((text != '') && (text != null) && (text != "undefined")) {
        console.log("submitting datastring: " + dataString);
        $.ajax({
          dataType: "json",
          type: "POST",
          url: "",
          data: dataString,
          success: function(data) {
            console.log("successfully submitted form");
            console.log("return value: " + data);
            console.log("type: " + (typeof data));
            var array = data;
            console.log("length: " + (array.length));
            var c1 = array[array.length - 1];
            console.log("c1 text: " + c1.text + "; text: " + text);
            var form = "<form action=\"\" id=\"commentForm\" method=\"POST\"> <input type=\"hidden\" id=\"author\" name=\"author\" value=" + author + "> <input type=\"hidden\" id=\"post\" name=\"post\" value=" + pid + "> <div> Post a reply <br> <input type=\"text\" id=\"text\" name=\"text\"></div> <input type=\"hidden\" id=\"parent\" value=" + par + "> <p><input type=\"submit\" class=\"submit\" id=\"submit\" value=\"Post\"</p>"
            var html = "<ul><h3>" + c1.author + "</h3>" + c1.text;
            if (par == "") {
              console.log("no parent");
              $("#root").prepend(html + form);
            } else {
              console.log("parent: " + par);
              $("#"+par).append(html + form);
            }
            return data;
          },
        });
      }
      return false;
    });
  });
*/

