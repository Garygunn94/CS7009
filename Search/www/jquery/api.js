
var getSocialGraph = function(onSuccess, onError)
{
  $.ajax(
    { url: '/socialGraph'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
