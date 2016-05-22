boardController = ->
    view = undefined
    history = undefined

    startLocations = {
        "Red": {
            "position": { "x": 3, "y": 0 }
            "direction": "North"
            "layer": 0
        }
        "Pink": {
            "position": { "x": 0, "y": 3 }
            "direction": "North"
            "layer": 0
        }
        "Yellow": {
            "position": { "x": 6, "y": 3 }
            "direction": "North"
            "layer": 0
        }
        "Blue": {
            "position": { "x": 3, "y": 12 }
            "direction": "South"
            "layer": 0
        }
        "Purple": {
            "position": { "x": 6, "y": 9 }
            "direction": "South"
            "layer": 0
        }
        "Green": {
            "position": { "x": 0, "y": 9 }
            "direction": "South"
            "layer": 0
        }
        "Ball": {
            "position": { "x": 3, "y": 6 }
            "direction": "North"
            "layer": 0
        }
    }

    currentLocations = startLocations

    initialize = ->
        view = boardView()
        view.initialize(startLocations)
        resetHistory()

    resetHistory = ->
        history = []

    animateMove = (action, callback) ->
        previous = {}
        for robot, location of action
            previous[robot] = currentLocations[robot]
            currentLocations[robot] = location
        history.push(previous)
        view.animateMove action, callback

    return {initialize, animateMove}
