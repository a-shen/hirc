
$(document).ready(function() {
  
  $("#commentForm").submit(function() { // commentForm is for new comment
    var dataString = $("#commentForm").serialize();
    var pid = $("#post").val();
    var text = $("#text").val();
    if ((text != '') && (text != null) && (text != "undefined")) {
      $.ajax({
        dataType: "json",
        type: "POST",
        url: "",
        data: dataString,
        success: function(data) {
          var array = data;
          var newcomment = array[array.length - 1];
          display_comment(newcomment, "#root");
          return data;
        },
      });
    }
    return false;
  });
  $(".reply-button").click(function() {
    $(this).hide();
    var parent = $(this).parent()[0]; // returns a div
    handle_reply(parent);
  });
});

/**
Append the comment and a reply button to the destination div
*/
function display_comment(comment, destination) {
  var cid = comment._id;
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var head = '<div class="comment" id=' + cid + '>';
  if ((destination != "#root") && ($(destination).parent() == "#root")) {
    head = '<div class="reply" id=' + cid + '>';
  } 
  if (destination == "#root") {
    head = '<h6>line break</h6><div class="comment" id=' + cid + '>';
  }
  var html = // comment plus reply button
  $(head +
    '<h3>' + comment.author + '</h3>' +
    '<p>' + date + '</p>' +
    '<blockquote>' + comment.text.replace(/\r?\n/g, '<br />') + '</blockquote>' +
    '<button class="reply-button2">Reply</button><br>' +
    '</div>').appendTo(destination);
  $(destination).append(html);

  $(".reply-button2").click(function() {
    var parent = $(this).parent()[0];
    handle_reply(parent);
    $(this).remove();
  });
}

/**
Display a form allowing user to submit a reply, and provide a callback function
*/
function handle_reply(parent) {
  var username = $("#username").text();
  var id = parent.id; // in reply to
  var url = document.URL.split("/");
  if (url.length < 2) {
    return false;
  }
  var postid = url[url.length - 2]; // url is lbh.org/postid/comments
  var form =
  $('<form action="#">'+
    '<input type="hidden" name="parent" value=\"' + id + '\"/>'+
    '<input type="hidden" name="post" value=\"' + postid + '\"/>'+
    '<input type="hidden" name="author" value=\"' + username + '\"/>'+
    '<textarea name="text"></textarea><br>'+ 
    '<input type="submit" value="Reply"/>'+
    '</form>').appendTo(parent)
  form.submit(function(event) {
    event.preventDefault();
    var dataString = form.serialize();
    var f = $(this);
    $.ajax({
      dataType: "json",
      type: "POST",
      url: "",
      data: dataString,
      success: function(data) {
        var array = data;
        if (array.length <= 0) {
          return false;
        }
        var newcomment = array[array.length - 1];
        display_comment(newcomment, "#"+newcomment.parent);
        f.remove();
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
  var h = d.getHours();
  var min = addZero(d.getMinutes());
  var mon = addZero(d.getMonth() + 1);
  var day = addZero(d.getDate());
  return d.getFullYear() + "-" + mon + "-" + day + " " + h + ":" + min;
}

function addZero(n) { // to match the haskell format
  if (n < 10) {
    n = "0" + n;
  }
  return n;
}

