
var getSocialGraphByUsername = function(username, onSuccess, onError)
{
  $.ajax(
    { url: '/socialGraph/' + encodeURIComponent(username) + ''
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

var getRepoSizeChart = function(onSuccess, onError)
{
  $.ajax(
    { url: '/RepoSizeChart'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
