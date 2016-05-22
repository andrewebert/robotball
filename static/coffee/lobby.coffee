app = angular.module('lobbyApp', ['socket'])
    
app.controller 'LobbyCtrl', ['$scope', '$window', 'socket',
    ($scope, $window, socket) ->
        $scope.id = undefined
        $scope.gotPlayers = false
        $scope.players = []

        socket.onmessage (data) ->
            console.log data
            method = data.method
            if method == 'setId'
                $scope.id = data.userId
            else if method == 'setPlayers'
                $scope.players = data.players
                $scope.gotPlayers = true
            else if method == 'addPlayer'
                if $scope.gotPlayers
                    $scope.players.push({userId: data.userId, name: data.name})
            else if method == 'removePlayer'
                $scope.players = (player for player in $scope.players when \
                    player.userId != data.userId)
            else if method == 'startGame'
                console.log("Started game with " + data.opponent)
                $window.location.href = "#{$window.location.origin}/game/#{$scope.id}"
            return

        $scope.startGame = (opponentId) ->
            socket.send {method: "startGame", opponent: opponentId}

        $scope.setUsername = (username) ->
            socket.send {method: "setName", name: username}

        $window.onbeforeunload = ->
            socket.send {method: "leave"}
]
