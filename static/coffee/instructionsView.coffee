# returns ()
instructionsView = (instructions, bufferLabels, opponentBufferLabels,
                    getInstructionAt, setInstructionAt) ->
    # constants
    instructionWidth = 80
    instructionHeight = Math.floor(instructionWidth * 110 / 130)
    instructionBoxHeight = Math.floor(instructionWidth * (110-25) / 130 )
    gridLeft = 20
    gridTop = 380
    bufferWidth = 80
    labelHeight = instructionBoxHeight
    bufferHeight = labelHeight + 3*instructionBoxHeight + instructionHeight
    bufferLeft = 45
    bufferTop = 50
    gridSpacing = 20
    bufferSpacing = 45
    placeholderWidth = 25
    placeholderHeight = 25

    dimensions = {
        instructionWidth
        instructionHeight
        instructionBoxHeight
        bufferWidth
        bufferHeight
        labelHeight
        placeholderWidth
        placeholderHeight
    }

    gridCellWidth = instructionWidth + gridSpacing
    gridCellHeight = instructionHeight + gridSpacing
    gridLocations = {}
    for row, y in instructions
        for ins, x in row
            gridLocations[ins] = [gridLeft + x*gridCellWidth,
                                  gridTop + y*gridCellHeight]

    defaultBufferLocations = {}
    for buffer, i in bufferLabels
        x = bufferLeft + i * (bufferWidth + bufferSpacing)
        defaultBufferLocations[buffer] = [x, bufferTop]

    # state
    animation = undefined
    animationStage = "default"
    startingPlayer = undefined

    placeholders = {}
    canvas = undefined
    
    # instruction -> [realX, realY]
    getInstructionLocation = (instruction) ->
        return getInstructionLocations()[instruction]


    # () -> {instruction: [realX, realY]}
    getInstructionLocations = ->
        return gridLocations


    # () -> {buffer: [realX, realY]}
    getBufferLocations = ->
        allBufferLocations = ([i*bufferWidth, bufferTop] for i in [0..5])
        if animationStage == "showOpponent"
            locations = {}
            if startingPlayer
                left = bufferLabels
                right = opponentBufferLabels
            else
                left = opponentBufferLabels
                right = bufferLabels
            for buffer, i in left
                locations[buffer] = allBufferLocations[i*2]
            for buffer, i in right
                locations[buffer] = allBufferLocations[i*2+1]
            return locations
        else
            return defaultBufferLocations


    # () -> {buffer: [[realX, realY]]}
    getBufferCells = ->
        bufferCells = {}
        for buffer, [x,y] of getBufferLocations()
            bufferCells[buffer] = []
            for i in [0..3]
                cellX = x
                cellY = y + labelHeight + i*instructionBoxHeight
                bufferCells[buffer][i] = [cellX, cellY]
        return bufferCells

    startTurn = (starting) ->
        startingPlayer = starting
        showPlaceholdersIfReady()

    showPlaceholdersIfReady = ->
        for label in opponentBufferLabels
            if not placeholders[label]
                return
        if startingPlayer?
            showPlaceholders()
            return

    showPlaceholders = ->
        for i in [0...3]
            baseline = defaultBufferLocations[bufferLabels[i]][0]
            if startingPlayer
                x = baseline + bufferWidth + (bufferSpacing-placeholderWidth)/2
            else
                x = baseline - bufferSpacing + (bufferSpacing-placeholderWidth)/2
            y = bufferTop + (instructionBoxHeight-placeholderHeight)/2
            placeholders[opponentBufferLabels[i]].set {
                left: x
                top: y
                visible: true
            }
            canvas.renderAll()

    hidePlaceholders = ->
        for _, img of placeholders
            img.set {visible: false}

    initialize = ->
        setup = instructionsViewSetup(dimensions, instructions, opponentBufferLabels,
                getInstructionLocations, getBufferLocations)
        interaction = instructionsViewInteration(dimensions, getInstructionLocation,
                getBufferCells, getInstructionAt, setInstructionAt)
        animation = instructionsViewAnimation(dimensions, getInstructionLocation,
                getBufferLocations, getBufferCells, getInstructionAt)

        instructionCallback = (img, canvas) ->
            interaction.addInteraction img, canvas
            animation.registerInstruction img

        placeholderCallback = (buffer, img) ->
            placeholders[buffer] = img
            showPlaceholdersIfReady()

        canvas = setup.initialize(instructionCallback, animation.registerBuffer,
                placeholderCallback)

        animation.initialize(canvas)
        return

    placeInstruction =  (instruction, buffer, sequence)->
        animation.placeInstruction instruction, buffer, sequence

    startAnimation = (callback) ->
        animationStage = "showOpponent"
        hidePlaceholders()
        animation.startAnimation callback


    animateMove = (color, sequence, instruction, callback) ->
        animation.animateMove color, sequence, instruction, callback


    endAnimation = (callback) ->
        animationStage = "default"
        animation.endAnimation callback

    return {initialize, placeInstruction, startAnimation, animateMove,
        endAnimation, startTurn}
