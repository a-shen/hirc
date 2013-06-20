
$(document).ready(function() {

  $("#chats").load(document.URL);
  
  $("#chatForm").submit(function() {
    var dataString = $("#chatForm").serialize();
    if ((text != '') && (text != null) && (text != "undefined")) {
      $.ajax({
        dataType: "json",
        type: "POST",
        url: "",
        data: dataString,
        success: function(data) {
          var array = data;
          var newchat = array[array.length - 1];
          showChat(newchat);
          return data;
        },
      });
    }
    return false;
  });
});

/**
Append the new chat to the chats div
*/
function showChat(chat) {
  var cid = chat._id;
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var html = // comment plus reply button
  $('<p>' + "<" + date + ">" + '<b>' + chat.author + ": " + '</b>' + 
    chat.text + '</p>').appendTo("#chats");
  $("#chats").append(html);
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
