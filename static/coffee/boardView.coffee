boardView = ->
    boardLeft = 20
    boardTop = 100
    scale = 760/1062
    cellWidth = 80 * scale
    cellHeight = cellWidth
    robotWidth = 60 * scale
    robotHeight = robotWidth
    goalHeight = cellHeight
    borderWidth = 7 * scale

    boardHeight = ((cellHeight - borderWidth) * 15 + borderWidth)
    boardWidth = ((cellWidth - borderWidth) * 7 + borderWidth)
        
    directions = {
        "North": 0
        "East": 90
        "South": 180
        "West": 270
    }

    dimensions = {
        boardLeft,
        boardTop,
        boardWidth,
        boardHeight,
        robotWidth,
        robotHeight,
        directions
    }

    animation = undefined

    # Converts cell coordinates to the canvas location of a robot
    # Robot location is based on the center of the robot (to make rotation/scaling easier)
    cellLocation = (x, y) ->
        realX = boardLeft + (cellWidth-borderWidth)*x + cellWidth/2 
        realY = boardTop + (goalHeight-borderWidth) + (cellHeight-borderWidth)*(12-y) + cellHeight/2
        return [realX, realY]

    initialize = (startLocations) ->
        setup = boardViewSetup(dimensions, cellLocation)
        animation = boardViewAnimation(dimensions, cellLocation)
        canvas = setup.initialize(startLocations, animation.registerRobot)
        animation.initialize(canvas)

    animateMove = (move, callback) ->
        animation.animateMove move, callback

    return {initialize, animateMove}
