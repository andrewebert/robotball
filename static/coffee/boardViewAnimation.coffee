boardViewAnimation = (dimensions, cellLocation) ->
    canvas = undefined
    robots = {}

    initialize = (c) ->
        canvas = c

    registerRobot = (color, img) ->
        robots[color] = img

    setLayer = (img, layer) ->
        img.layer = layer
        imgs = (v for k,v of robots)
        imgs.sort (a, b) ->
            a.layer - b.layer
        for img, i in imgs
            img.moveTo(i+1)

    animateAction = (robot, action, callback) ->
        [x,y] = cellLocation action.position.x, action.position.y
        scale = Math.pow(0.7,action.layer)
        width = dimensions.robotWidth * scale
        height = dimensions.robotHeight * scale

        img = robots[robot]
        moveDuration = (Math.abs(img.left - x) + Math.abs(img.top - y)) * 5
        duration = Math.min(moveDuration, 500)

        # This is required to make the robot rotate 90 degrees counter-clockwise
        # instead of 270 degrees clockwise
        adjust = (angle, old_angle) ->
            normalize = (a) -> (a%360 + 360) % 360
            rotation = normalize(angle - old_angle)
            if rotation == 270
                rotation = -90
            return old_angle + rotation
        angle = adjust(dimensions.directions[action.direction], img.getAngle())

        if action.layer?
            setLayer(img, action.layer)

        img.animate {
            left: x
            top: y
            width: width
            height: height
            angle: angle
        }, {
            onChange: canvas.renderAll.bind(canvas)
            duration: duration
            onComplete: callback
        }
        return

    animateMove = (move, callback) ->
        animateAll callback, move, animateAction

    return {initialize, registerRobot, animateMove}
