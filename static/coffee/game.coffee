app = angular.module('gameApp', ['socket'])

STATE_WAITING = "state_waiting"
STATE_BUFFERS_FULL = "state_buffers_full"
STATE_FILLING_BUFFERS = "state_filling_buffers"
STATE_PROCESSING_TURN = "state_processing_turn"
STATE_PROCESSED_TURN = "state_processed_turn"

app.controller 'GameCtrl', ['$scope', '$location', '$timeout', 'socket',
    ($scope, $location, $timeout, socket) ->
        DEBUG = false
        IGNORE_SERVER = false

        $scope.redScore = 0
        $scope.blueScore = 0

        $scope.redGoal = false
        $scope.blueGoal = false

        $scope.rules = false

        $scope.win = false
        $scope.lose = false

        setState = (state) ->
            console.log state
            $scope.state = state
            #$scope.$apply()

        # fillingBuffers, buffersFull, processingTurn, processedTurn, waiting
        if DEBUG
            setState(STATE_BUFFERS_FULL)
        else
            setState(STATE_FILLING_BUFFERS)

        buffers = undefined
        startingPlayer = false

        buffersFull = (bufs) ->
            #console.log bufs
            buffers = bufs
            setState(STATE_BUFFERS_FULL)
            $scope.$apply()

        buffersNotFull = (bufs) ->
            if $scope.canReady
                console.log "buffers not full"
            if !DEBUG
                setState(STATE_FILLING_BUFFERS)
            buffers = bufs
            $scope.$apply()

        inst = instructionsController(buffersFull, buffersNotFull)
        board = boardController()
        inst.initialize()
        board.initialize()

        wait = (delay, callback) ->
            $timeout callback, delay

        $scope.canReady = ->
            if DEBUG
                return true
            else
                return $scope.state == STATE_BUFFERS_FULL or $scope.state == STATE_PROCESSED_TURN

        $scope.readyText = ->
            if $scope.state == STATE_BUFFERS_FULL or $scope.state == STATE_FILLING_BUFFERS
                return "Ready"
            else if $scope.state == STATE_WAITING
                return "Waiting"
            else
                return "Next Turn"

        $scope.ready = ->
            console.log buffers
            if (DEBUG and $scope.state == STATE_FILLING_BUFFERS) or $scope.state == STATE_BUFFERS_FULL
                if IGNORE_SERVER
                    test(inst, processTurn)
                else
                    socket.send buffers
                    setState(STATE_WAITING)
            else if $scope.state == STATE_PROCESSED_TURN
                inst.endAnimation ->
                    setState(STATE_FILLING_BUFFERS)
                    inst.startTurn startingPlayer
                    $scope.$apply()

        socket.onmessage (actions) ->
            console.log actions
            if "play" in (a.key for a in actions)
                processTurn actions
            else if actions[0].key == "startingPlayer"
                processActions actions, ->
                    inst.startTurn startingPlayer

        processTurn = (actions) ->
            setState(STATE_PROCESSING_TURN)
            inst.startAnimation ->
                wait 500, ->
                    processActions actions, ->
                        setState(STATE_PROCESSED_TURN)

        processActions = (actions, callback) ->
            delay = 500

            if actions.length == 0
                callback()
                return

            next = ->
                wait delay, ->
                    processActions actions[1..], callback

            action = actions[0]
            log action
            if action.key == "play"
                inst.animateMove action.value, next
            if action.key == "update"
                board.animateMove action.value, next
            if action.key == "startingPlayer"
                if action.value == "Red"
                    startingPlayer = true
                else
                    startingPlayer = false
                next()
            if action.key == "score"
                goal action.value, next
            return

        goal = (player, callback) ->
            if player == "Red"
                color = "redGoal"
                $scope.redScore += 1
                if $scope.redScore >= 3
                    $scope.win = true
                    # Don't call the callback, the game's over
                    return
            else
                color = "blueGoal"
                $scope.blueScore += 1
                if $scope.blueScore >= 3
                    $scope.lose = true
                    # Don't call the callback, the game's over
                    return
            $scope[color] = true
            wait 1000, ->
                $scope[color] = false
                callback()

        log = (action) ->
            if action.key == "play"
                console.log "play #{action.value.color}: #{action.value.card}"
            if action.key == "update"
                console.log "update"
                for color, update of action.value
                    if update.layer?
                        layer = ", layer #{update.layer}"
                    else
                        layer = ""
                    console.log "    #{color}: (#{update.position.x}, #{update.position.y})" +
                        " (#{update.direction}#{layer})"
            if action.key == "startingPlayer"
                console.log "Starting player: #{action.value}"
            if action.key == "score"
                console.log "#{action.value} goal!"

]
