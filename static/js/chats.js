
$(document).ready(function() {

  var nchats = 0; // number of chats currently indexed on the page

  function poll() {
    setTimeout(function() {
      $.ajax({
        url: document.URL,
        success: function(data) {
          console.log(data);
          for (var n = nchats; n < data.length; n++) {
            $("#chats").append(showChat(data[n]));
          }
          nchats = data.length;
          console.log("nchats: " + nchats);
          poll();
        }, dataType: "json"
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

/*
  $("#chanForm").submit(function() {
    alert("chan form submitted");
    var dataString = $("#chanForm").serialize();
    $.ajax({
      dataType: "json",
      type: "POST",
      url: "/channels",
      data: dataString,
      success: function(data) {
        console.log("success function called");
        var newchan = data[data.length - 1];
        var cid = newchan._id;
        console.log("id of new channel: " + cid);
        var notice = "Your channel will be located at: localhost:8080/" + cid + "/chats";
        if (newchan.listed == "False") {
          notice = notice + "\nCopy and paste this link and give it to the people who you are inviting to join your chat room."
        }
        alert(notice);
        return data;
      },
    });
    return true;
  });
*/

  $(window).unload(function() {
    // do something
  });

});


/**
Format the new chat to the chats div
*/
function showChat(chat) {
  var cid = chat._id;
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var html = // comment plus reply button
  $('<p>' + "<" + date + ">" + '<b>' + chat.author + ": " + '</b>' + 
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

