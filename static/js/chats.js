
$(document).ready(function() {

  // update user list to include new user
  var cuser = $("#currentUser").text();
  var userHtml = '<div id="' + cuser + '">' + cuser + '</div>';
  $("#users").append(userHtml);
  $("#users").append("hi i'm a new user");
  console.log("user should've been appended");

  var last_id = "";  // id of the last chat on the page

  function poll() {
    setTimeout(function() {
      $.ajax({
        url: document.URL,
        success: function(data) {
          console.log("last_id = " + last_id);
          console.log("data: " + logData(data));
          var newind = 0;  // index of the first new chat
          for (var n = 0; n < data.length; n++) {
            console.log("cur id: " + data[n]._id);
            if (data[n]._id == last_id) {  // just found the last chat indexed on the page
              newind = n + 1;
              console.log("found newind: " + newind);
              break;
            }
          }
          for (var n = newind; n < data.length; n++) {
            console.log("appending chat: " + data[n]);
            $("#chats").append(showChat(data[n]));
            var allchats = $(".chat");
            var maxchats = 100;  // todo change to 100
            if (allchats.length > maxchats) {
              // remove one chat from the top to maintain a consistent number of chats on the page
              $(".chat").get(0).remove();
            }
          }
          last_id = data[data.length - 1]._id;
          poll();
        },
        dataType: "json"
      });
    }, 750);  // refresh every 0.75 sec
  }

  poll();

  $("#chatForm").submit(function() {
    var dataString = $("#chatForm").serialize();
    var text = $("#text").val();
    if ((text != '') && (text != null) && (text != "undefined")) {
      $.ajax({
        dataType: "json",
        type: "POST",
        url: "",
        data: dataString,
        success: function(data) {
          var newchat = data[data.length - 1];
          $("#text").val("");
          poll();
          return data;
        },
      });
    }
    return false;
  });

  $(window).unload(function() {  // user left, so remove their name from the list
    console.log("removing cuser: " + cuser);
    $("#"+cuser).remove();
  });

});


/**
Returns the formatted chat ready to be appended to the chats div
*/
function showChat(chat) {
  var cid = chat._id;
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var html =
  $('<p class="chat">' + date + '&nbsp' + '<b>' + chat.author + ": " + '</b>' +
    chat.text + '</p>').appendTo("#chats");
}

/**
Format the date to something like "16:31:09"
*/
function formatDate(d) {
  var h = d.getHours();
  var m = addZero(d.getMinutes());
  var s = addZero(d.getSeconds());
  return h + ":" + m + ":" + s;
}

function addZero(n) { // to match the haskell format
  if (n < 10) {
    n = "0" + n;
  }
  return n;
}

function logData(array) { // for debugging purposes
  var str = "";
  for (var i = 0; i < array.length; i++) {
    // str += array[i].text + " ";
    str += array[i]._id + " ";
  }
  return str;
}

