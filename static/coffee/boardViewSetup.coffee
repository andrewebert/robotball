boardViewSetup = (dimensions, cellLocation) ->
    initialize = (startLocations, callback) ->

        canvas = new fabric.Canvas 'board', {
            width: dimensions.boardWidth + (dimensions.boardLeft * 2)
            height: dimensions.boardHeight + dimensions.boardTop
            selection: false
        }

        canvas.on 'mouse:down', (options) ->
            console.log 'test2'

        fabric.Image.fromURL '/static/img/board.png', (img) ->
            img.set {
                left: dimensions.boardLeft
                top: dimensions.boardTop
                width: dimensions.boardWidth
                height: dimensions.boardHeight
                selectable: false
            }
            canvas.add img
            img.moveTo(0)

        placeRobots canvas, startLocations, callback
        return canvas

    placeRobots = (canvas, startLocations, callback) ->
        placeRobot = (robot, x, y, angle) ->
            fabric.Image.fromURL "/static/img/robots/#{robot}.png", (img) ->
                img.set {
                    left: x
                    top: y
                    width: dimensions.robotWidth
                    height: dimensions.robotHeight
                    angle: angle
                    selectable: false
                    originX: "center"
                    originY: "center"
                    layer: 0
                }
                canvas.add img
                callback robot, img

        for robot, data of startLocations
            [realX, realY] = cellLocation data.position.x, data.position.y
            placeRobot robot, realX, realY, dimensions.directions[data.direction]
        return

    return {initialize: initialize}

