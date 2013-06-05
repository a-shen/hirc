$(document).ready(function()
{
  alert("welcome");
  $("#submit").click(function() {
    alert("submit was clicked");
    var n = $("form").length;
    alert("n = " + n);

    for (var i = 0; i < n; i++) {
      var element = $("form:eq(i)");
      var text = $("#text").val();
      alert("got comment text");
      alert("comment text: " + text);
      if (text != "") {
        //var dataString = $("form:eq(i)").serialize();
        var dataString = $(element).serialize();
        var txt = $(element).find("#text");
        alert("txt: " + txt);
        var pid = $("#post").val();
        alert("submitting datastring: " + dataString);
        $.ajax({
          type: "POST",
          url: "",
          data: dataString,
          success: function(html) {
            alert("successfully submitted form");
            $("#commentList").append(html);
            alert("appended comment");
          }
        });
      }
      return false;
    }
  });
});



/*
  var element = $(this);
  var text = $("#text").val();
  var pid = $("#post").val();
  var author = $("#author").val();
  var par = $("#parent").val();
*/
