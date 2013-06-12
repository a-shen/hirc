
$(document).ready(function() {
  
  $("#commentForm").submit(function() { // commentForm is for new comment
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

/**
Append the comment and a reply button to the destination div
*/
function display_comment(comment, destination) {
  //console.log("dest: " + destination);
  var cid = comment._id;
  //console.log("id: " + cid);
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date( parseInt( timestamp, 16 ) * 1000 ));
  var head = '<div class="comment" id=' + cid + '>';
  if ((destination != "#root") && ($(destination).parent() == "#root")) {
    head = '<div class="reply" id=' + cid + '>';
  } 
  if (destination == "#root") {
    console.log("destination is root; adding space");
    head = '<h6>line break</h6><div class="comment" id=' + cid + '>';
  }
  var html = // comment plus reply button
  $(head +
    '<h3>' + comment.author + '</h3>' +
    '<p>' + date + '</p>' +
    '<blockquote>' + comment.text.replace(/\r?\n/g, '<br />') + '</blockquote>' +
    '<button class="reply-button2">Reply</button><br>' +
    '</div>').appendTo(destination);
    //'</div><h6>Line break</h6>').appendTo(destination);
  $(destination).append(html);

  $(".reply-button2").click(function() {
    console.log("reply-button2 clicked");
    var parent = $(this).parent()[0]; // returns a div
    handle_reply(parent);
    $(this).remove();
  });
}

/**
Display a form allowing user to submit a reply, and provide a callback function
*/
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
      '<textarea name="text"></textarea><br>'+ 
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

/**
Format the date to something like "2013-06-10 16:31"
*/
function formatDate(d) {
  console.log("formatting date: " + d);
  var h = d.getHours();
  var min = addZero(d.getMinutes());
  var mon = addZero(d.getMonth() + 1);
  var day = addZero(d.getDate());
  return d.getFullYear() + "-" + mon + "-" + day + " " + h + ":" + min;
}

function addZero(n) {
  if (n < 10) {
    n = "0" + n;
  }
  return n;
}

