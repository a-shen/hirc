
$(document).ready(function() {
  $("#commentForm").submit(function() { // making new comment
    //console.log("submit was clicked");
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
          //console.log("return value: " + data);
          var array = data;
          console.log("data: " + (array.join()));
          var c1 = array[array.length - 1];
          //console.log("c1 text: " + c1.text + "; text: " + text);
          display_comment(c1, "#root");
          return data;
        },
      });
    }
    return false;
  });
  $(".reply-button").click(function() {
    console.log("reply-button clicked");
    $(this).hide();
    var parent = $(this).parent()[0]; // returns a div
    handle_reply(parent);
  });
});

function display_comment(c1, destination) { // append comment and a reply button to destination div
  //console.log("dest: " + destination);
  var cid = c1._id;
  //console.log("id: " + cid);
  var timestamp = cid.toString().substring(0,8);
  date = new Date( parseInt( timestamp, 16 ) * 1000 );
  var head = '';
  var end = '</div>';
  if ((destination != "#root") && ($(destination).parent() == "#root")) { // if it's a reply, indent it
    head = '<ul><div class="comment" id=' + cid + '>';
    end = end + '</ul>';
    //head = '<div class="reply" id=' + cid + '>';
  } else {
    head = '<div class="comment" id=' + cid + '>';
  }
  var html = // comment with reply button
  $(head +
    '<h3>' + c1.author + '</h3>' +
    '<p>' + date + '</p>' +
    '<blockquote>' + c1.text + '</blockquote>' +
    '<button class="reply-button2">Reply</button>' +
    end).appendTo(destination);
  //console.log("no parent");
  $(destination).append(html);
  //console.log("added comment and button");
  $(".reply-button2").click(function() {
    console.log("reply-button2 clicked");
    var parent = $(this).parent()[0]; // returns a div
    handle_reply(parent);
    $(this).remove();
  });
}

function handle_reply(par) {
    var username = $("#username").text();
    console.log("username: " + username);
    console.log("handle_reply called");
    console.log("parent: " + par);
    var id = par.id; // in reply to
    console.log("par.id: " + par.id);
    var url = document.URL.split("/");
    var post = url[url.length - 2];
    console.log("post: " + post);
    var form =
    $('<form action="#">'+
      '<input type="hidden" name="parent" value=\"' + id + '\"/>'+
      '<input type="hidden" name="post" value=\"' + post + '\"/>'+
      '<input type="hidden" name="author" value=\"' + username + '\"/>'+
      '<textarea name="text"> </textarea><br>'+
      '<input type="submit" value="Reply"/>'+
      '</form>').appendTo(par);
    console.log("made form");
    form.submit(function(event) {
      event.preventDefault();
      var dataString = form.serialize();
      console.log("sending request : " + dataString);
      var f = $(this);
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
          //var destination = $("#"+c1.parent);
          display_comment(c1, "#"+c1.parent);
          f.remove();
          console.log("removed form");
          return false;
        }
      })
      return false;
    });
}

