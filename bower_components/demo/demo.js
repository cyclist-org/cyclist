$(document).ready(function() {
    $("#submitBtn").click(function() {
        if($("#visualisation").length) { //remove previous image if any
            $("#visualisation").remove();
        }
        if($("#output").length) { //remove previous output if any
            $("#output").remove();
        }
        if ($("#sequent").parent().hasClass("has-error")) {
            $("#sequent").parent().removeClass("has-error");
            $(".alert").remove();
        }
        if(trimfield($("#sequent").val()) == '') { //if input is empty on submit
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
                if(json_obj.entailment == null) { //no std output: error, invalid sequent
                    var parent = $("#sequent").parent();
                    parent.addClass("has-error");
                    parent.append("<div style=\"margin: 5px 0; padding: 3px;\"class=\"alert alert-danger\" role=\"alert\">Invalid sequent!</div>");
                    return;
                }
                style = "";
                if(json_obj.entailment.match("^Proved")) {
                    style = "color: green;"
                } else if(json_obj.entailment.match("^NOT")){
                    style = "color: red;"
                }
                if($("#output").length) {
                    $("#output").replaceWith("<div id=\"output\"><p "+ "style=\"margin: 20px 0; "+ style +"\">" + json_obj.entailment + "</p></div>")
                } else {     
                    $("#prover").append("<div id=\"output\"><p id=\"output\" "+ "style=\"margin: 20px 0; "+ style +"\">" + json_obj.entailment + "</p></div>");
                }
                if(json_obj.entailment.match("^Proved") && !(json_obj.visualisation === undefined)) {
                    visualise('http://sciencesoft.at/latex/compile', { ochem: 'false',
                                runexample: 'false',
                                src: json_obj.visualisation,
                                dpi: '120',
                                utf8: 'on',
                                template: '0en',
                                device: '0',
                                papersize: '0' });
                }
                if(!(json_obj.latex === undefined)) {
                    download("proof.tex", json_obj.latex);
                }
            }
        });
    });
});

function visualise(u,data){
    $('body').append('<form method="post" id="params"></form>');
    $.each(data,function(n,v){
        $('#params').append('<input type="hidden" name="'+n+'" value="'+ v.replace(/"/g, '&quot;') +'" />');
    });
    $('#params').append('<input type="hidden" name="url" value="'+ u +'" />'); //needed by proxy: where to redirect request

    //UCL-CS webserver acts as a proxy in order to make CORS request possible
    proxy = 'https://students.cs.ucl.ac.uk/2015/group33/prover/proxy.php';

    // Make JSON request.
    $.ajax({
        type: "POST",
        url: proxy,
        data: $('#params').serialize(),
        success: function(response) {
            var json = $.parseJSON(response);//parse JSON
            if(json.contents === undefined) {
                return;
            }
            $("#prover").append('<img id=\"visualisation\" class=\"center-block\" src="http://sciencesoft.at/image/latex?index='+ json.contents.idx +'&id='+ json.contents.id + '" alt="Proof Graph">');
        },
        dataType: 'text'
        /*error: function(a, b, c) {
            alert(b + c);
        }*/
    });
}


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