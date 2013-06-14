
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
    var parent = $(this).parent()[0]; // returns a div
    handle_reply(parent);
    $(this).remove();
  });
  $(".edit-button").click(function() {
    //console.log("edit-button clicked");
    var parent = $(this).parent()[0];
    console.log("parent: " + parent);
    console.log("grandparent: " + $(parent).parent());
    handle_edit(parent);
    $(this).remove();
  });
});

/**
Display a form allowing user to edit a comment, and provide a callback function
*/
function handle_edit(oldcomment) {
  var username = $("#username").text();
  if (username == "Anonymous") {
    return false;
  }
  var id = oldcomment.id;
  var url = document.URL.split("/");
  if (url.length < 2) {
    return false;
  }
  var postid = url[url.length - 2]; // url is x.org/postid/comments
  var parent = $("#p"+id).text();
  var pinput = '<input type="hidden" name="parent" value=\"' + parent + '\"/>'
  //if (parent == "") {
    //console.log("no parent");
    //pinput = '';
  //}
  console.log("parent of edited post: " + parent);
  var form =
  $('<form action="#">'+
    '<input type="hidden" name="_id" value=\"' + id + '\"/>'+
    '<input type="hidden" name="method" value="PUT"/>' +
    pinput +
    '<input type="hidden" name="post" value=\"' + postid + '\"/>'+
    '<input type="hidden" name="author" value=\"' + username + '\"/>'+
    '<textarea name="text"></textarea><br>'+
    '<input type="submit" value="Edit"/>'+
    '</form>').appendTo(oldcomment);

  form.submit(function(event) {
    event.preventDefault();
    var dataString = form.serialize();
    var f = $(this);
    $.ajax({
      dataType: "json",
      type: "PUT",
      url: "",
      data: dataString,
      success: function(data) {
        if (data.length <= 0) {
          return false;
        }
        var newcomment = data[data.length - 1];
        for (var n = data.length - 1; n >= 0; n--) { // most of the time it'll be the last one
          if (data[n]._id == id) {
            newcomment = data[n];
          }
        }
        replace_comment(newcomment);
        f.remove();
        return false;
      }
    })
    return false;
  });
}

/**
Replace the old comment with the updated comment
*/
function replace_comment(comment) {
  var cid = comment._id;

  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var head = '<div class="comment" id=' + cid + '>';
  //var buttons = '';
  var buttons = '<button class="reply-button">Reply</button><br>';
  console.log("buttons: " + buttons);
  var username = $("#username").text();
  if (username == comment.author && username != "Anonymous") {
    buttons = '<button class="edit-button2">Edit</button><br>' + buttons;
    console.log("buttons: " + buttons);
  }
  var destination = $("#"+cid)[0].firstChild;
  if (comment.parent != undefined && comment.parent != "" && comment.parent != null) {
    console.log("comment.parent = " + comment.parent);
    console.log("destination = #cid");
    destination = "#"+cid;
  }

/*
  console.log("comment.parent: " + $(comment).parent());
  if ($(comment).parent() != "root") {
    console.log("destination = #cid");
    destination = "#"+cid;
  }
*/

  var html = // comment plus reply button
  $(head +
    '<h3>' + comment.author + '</h3>' +
    '<p>' + date + '</p>' +
    '<blockquote>' + comment.text.replace(/\r?\n/g, '<br />') + '</blockquote>' +
    '<li>' + comment.parent + '</li>' +
    buttons +
    '</div>').appendTo(destination);
  console.log(destination);
  $(destination).replaceWith(html);

  $(".reply-button").click(function() {
    var parent = $(this).parent()[0];
    handle_reply(parent);
    $(this).remove();
  });
  $(".edit-button2").click(function() {
    //console.log("edit-button2 clicked");
    var parent = $(this).parent()[0]; // returns a div
    handle_edit(parent);
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
    '</form>').appendTo(parent);
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
Append the comment, a reply button, and an edit button if applicable to the destination div
*/
function display_comment(comment, destination) {
  var cid = comment._id;
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var head = '<div class="comment" id=' + cid + '>';
  var buttons = '<button class="reply-button2">Reply</button><br>';
  var username = $("#username").text();
  if (username == comment.author && username != "Anonymous") {
    buttons = '<button class="edit-button2">Edit</button><br>' + buttons;
  }
  var html = // comment plus reply button
  $(head +
    '<h3>' + comment.author + '</h3>' +
    '<p>' + date + '</p>' +
    '<blockquote>' + comment.text.replace(/\r?\n/g, '<br />') + '</blockquote>' +
    '<li>' + comment.parent + '</li>' +
    buttons +
    '</div>').appendTo(destination);
  $(destination).append(html);

  $(".reply-button2").click(function() {
    var parent = $(this).parent()[0];
    handle_reply(parent);
    $(this).remove();
  });
  $(".edit-button2").click(function() {
    //console.log("edit-button2 clicked");
    var parent = $(this).parent()[0]; // returns a div
    handle_edit(parent);
    $(this).remove();
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

