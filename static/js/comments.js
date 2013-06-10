

$(document).ready(function() {
  $("#commentForm").submit(function() { // making new comment
    console.log("submit was clicked");
    var dataString = $("#commentForm").serialize();
    var pid = $("#post").val();
    var text = $("#text").val();
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
          console.log("data: " + (array.join()));
          var c1 = array[array.length - 1];
          console.log("c1 text: " + c1.text + "; text: " + text);
          var cid = c1._id;
          console.log("id: " + cid);
          var html = // comment with reply button
          $('<div class="comment" id=' + cid + '>' +
            '<blockquote>' + text + '</blockquote>' +
            '<button class="reply-button">Reply</button>' +
            '</div>').appendTo("#root");
          console.log("no parent");
          $("#root").prepend(html);
          console.log("added comment and button");
          $(".reply-button").click(function() {
            var parent = $(this).parent()[0]; // returns a div
            console.log("parent in clic function: " + parent);
            handle_reply(parent);
          });
          return data;
        },
      });
    }
    return false;
  });

  $(".reply-button").click(function() {
    var parent = $(this).parent()[0]; // returns a div
    console.log("parent in clic function: " + parent);
    handle_reply(parent);
  });
});

function handle_reply(par) {
    console.log("reply button clicked");
    console.log("parent: " + par);
    var id = par.id; // in reply to
    console.log("par.id: " + par.id);
    //var post = parent.attr("post"); // the post it belongs to
    //var post = $("#commentForm").attr("post"); // TODO this will prob fail
    //var post = '516dc8b413c61405cb000000';
    var url = document.URL.split("/");
    var post = url[url.length - 2];
    console.log("post: " + post);
    var author = "Anon" // TODO
    var button = $(par);
    $(this).remove();
    var form =
    $('<form action="#">'+
      '<input type="hidden" name="parent" value=\"' + id + '\"/>'+
      '<input type="hidden" name="post" value=\"' + post + '\"/>'+
      '<input type="hidden" name="author" value=\"' + author + '\"/>'+
      '<input type="text" name="text" placeholder="Comment..."/>'+
      '<input type="submit" value="Reply2"/>'+
      '</form>').appendTo(par);
    console.log("made form");
    form.submit(function(event) {
      event.preventDefault();
      var dataString = form.serialize();
      alert("sending request : " + dataString);
      $.ajax({
        dataType: "json",
        type: "POST",
        url: "",
        data: dataString,
        success: function(data) {
          console.log("return value: " + data);
          var array = data;
          console.log("length: " + (array.length));
          var c1 = array[array.length - 1];
          var destination = $("#"+c1.parent);
          var text = c1.text;
          console.log("c1 text: " + text);
          console.log("parent: " + par);
          var html = "<blockquote>" + text + "</blockquote>";
          $(html).appendTo(destination);
          alert("reply form submitted.");
          button.show();
          $(this).remove();
          return false;
        }
      })
      return false;
    });
}

