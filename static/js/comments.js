
// <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js"></script> 

$(document).ready(function() { 
    var options = { 
        target: '#commentList',
        beforeSubmit: showRequest,
        success: showResponse
    };
 
    $('#commentForm').submit(function() {
        $(this).ajaxSubmit(options);
        // alert("ajaxSubmit");
        return false;
    });
});
 
function showRequest(formData, jqForm, options) {
    var queryString = $.param(formData);
    // alert("About to submit: \n\n" + queryString);
    return true;
}
 
function showResponse(responseText, statusText, xhr, $form)  {
    // alert('status: ' + statusText + '\n\nresponse: \n' + responseText + 
        '\n\nThe output div should be updated with the new comment.');
}
