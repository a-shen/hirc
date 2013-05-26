
// <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js"></script> 

// prepare the form when the DOM is ready
$(document).ready(function() { 
    var options = { 
        target: '#commentList',
        beforeSubmit:  showRequest,
        success:       showResponse
    }; 
 
    // bind to the form's submit event
    $('#commentForm').submit(function() {
        $(this).ajaxSubmit(options);
        return false;
    });
});
 
// pre-submit callback
function showRequest(formData, jqForm, options) {
    var queryString = $.param(formData);
    alert('About to submit: \n\n' + queryString);
    return true;
}
 
// post-submit callback
function showResponse(responseText, statusText, xhr, $form)  {
    alert('status: ' + statusText + '\n\nresponseText: \n' + responseText + 
        '\n\nThe output div should have already been updated with the responseText.');
}
