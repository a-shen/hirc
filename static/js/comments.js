
$(document).ready(function() { 
    var options = { 
        target: '#commentList',
        beforeSubmit: showRequest,
        success: showResponse
        //clearForm: true
        //resetForm: true
    };
 
    $('#commentForm').submit(function(event) {
        $(this).ajaxSubmit(options);
        alert("ajaxSubmit");
        event.preventDefaults();
        return false;
    });
});
 
function showRequest(formData, jqForm, options) {
    var queryString = $.param(formData);
    alert("About to submit: \n\n" + queryString);
    return true;
}
 
function showResponse(responseText, statusText, xhr, $form)  {
    alert('status: ' + statusText + '\n\nresponse: \n' + responseText + '\n\nThe output div should be updated with the new comment.');
}
