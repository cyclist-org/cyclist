$(document).ready(function() {
    $("#submitBtn").click(function() {
        if ($("#sequent").parent().hasClass("has-error")) {
            $("#sequent").parent().removeClass("has-error");
            $(".alert").remove();
        }
        if(trimfield($("#sequent").val()) == '') {
            if($("#output").length) {
                $("#output").remove();
            }
            var parent = $("#sequent").parent();
            parent.addClass("has-error");
            parent.append("<div style=\"margin: 5px 0; padding: 3px;\"class=\"alert alert-danger\" role=\"alert\">Insert sequent!</div>");
            return;
        }
        $("#submitBtn").wrap("<fieldset style=\"display: inline;\" disabled></fieldset>");
        $.ajax({
            url: 'https://students.cs.ucl.ac.uk/2015/group33/prover/script.php',
            type: 'post',
            dataType: 'json',
            xhrFields: {
                // The 'xhrFields' property sets additional fields on the XMLHttpRequest.
                // This can be used to set the 'withCredentials' property.
                // Set the value to 'true' if you'd like to pass cookies to the server.
                // If this is enabled, your server must respond with the header
                // 'Access-Control-Allow-Credentials: true'.
                withCredentials: false
            },
            data: $("#proverForm").serialize(),
            success: function(data) {
                $("#submitBtn").unwrap();
                var json_obj = $.parseJSON(JSON.stringify(data));//parse JSON
                if(json_obj.entailment == null) {
                    var parent = $("#sequent").parent();
                    parent.addClass("has-error");
                    parent.append("<div style=\"margin: 5px 0; padding: 3px;\"class=\"alert alert-danger\" role=\"alert\">Invalid sequent!</div>");
                    return;
                }
                if($("#output").length) {
                    $("#output").replaceWith("<p id=\"output\">" + json_obj.entailment + "</p>")
                } else {     
                    $("#proverForm").append("<p id=\"output\">" + json_obj.entailment + "</p>");
                }
                if(!(json_obj.latex === undefined)) {
                    download("proof.tex", json_obj.latex);
                }
            }
        });
    });
});

function trimfield(str) 
{ 
    return str.replace(/^\s+|\s+$/g,''); 
}

function download(filename, text) {
  var element = document.createElement('a');
  element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
  element.setAttribute('download', filename);

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}