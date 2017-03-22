
var getSocialGraph = function(onSuccess, onError)
{
  $.ajax(
    { url: '/socialGraph'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getLanguageChart = function(onSuccess, onError)
{
  $.ajax(
    { url: '/languageChart'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
