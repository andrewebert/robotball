angular.module('socket', []).factory 'socket', ($rootScope) ->
    root_url = /\/\/([^/]*)\/?/.exec(document.URL)[1]
    socket_url = "ws://#{root_url}/"
    socket = new WebSocket(socket_url)
    {
        onmessage: (callback) ->
            socket.onmessage = (message) ->
                $rootScope.$apply ->
                    callback(JSON.parse(message.data))

        send: (message) ->
            socket.send(JSON.stringify(message))
    }

        
