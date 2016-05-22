angular.module('socket', []).factory 'socket', ($rootScope) ->
    #root_url = /\/\/([^/]*)\/?/.exec(document.URL)[1]
    root_url = "#{window.location.host}#{window.location.pathname}"
    socket_url = "ws://#{root_url}"
    socket = new WebSocket(socket_url)
    {
        onmessage: (callback) ->
            socket.onmessage = (message) ->
                $rootScope.$apply ->
                    callback(JSON.parse(message.data))

        send: (message) ->
            socket.send(JSON.stringify(message))
    }

        
