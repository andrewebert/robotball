instructionsController = (fullCallback, nonFullCallback) ->
    instructions = [
                    ["North1", "North2", "North3", "Steal"],
                    ["South1", "South2", "South3", "Escape"],
                    ["East1", "East2", "West1", "West2"],
                    ["Grab1", "Grab2", "Throw3", "Throw4"],
                ]

    bufferLabels = ["Red", "Pink", "Yellow"]
    opponentBufferLabels = ["Blue", "Purple", "Green"]
    buffers = {}
    animated = {}

    view = undefined

    getInstructionAt = (buffer, sequence) ->
        if buffers[buffer]
            return buffers[buffer][sequence]
        else
            return undefined

    setInstructionAt = (instruction, buffer, sequence) ->
        if buffer
            buffers[buffer][sequence] = instruction
        else
            for l, b of buffers
                for i, s in b
                    if i == instruction
                        buffers[l][s] = undefined

        if buffersFull()
            fullCallback(buffers)
        else
            nonFullCallback(buffers)

        return

    emptyBuffers = ->
        for label in bufferLabels
            buffers[label] = [undefined, undefined, undefined, undefined]

    buffersFull = ->
        for label, instructions of buffers
            for instruction in instructions
                if not instruction
                    return false
        return true

    initialize = ->
        emptyBuffers()
        #nonFullCallback(buffers)
        view = instructionsView(instructions, bufferLabels, opponentBufferLabels,
                getInstructionAt, setInstructionAt)
        view.initialize()

    placeInstruction = (instruction, buffer, sequence) ->
        view.placeInstruction instruction, buffer, sequence
        setInstructionAt instruction, buffer, sequence

    startAnimation = (callback) ->
        for b in bufferLabels
            animated[b] = 0
        for b in opponentBufferLabels
            animated[b] = 0
        view.startAnimation callback

    animateMove = (action, callback) ->
        sequence = animated[action.color]
        animated[action.color] += 1
        view.animateMove action.color, sequence, action.card, callback
    
    endAnimation = (callback) ->
        for color in animated
            animated[color] = 0
        view.endAnimation ->
            emptyBuffers()
            callback()

    startTurn = (startingPlayer) ->
        view.startTurn(startingPlayer)

    return {initialize, placeInstruction, startAnimation, animateMove,
        endAnimation, startTurn}
