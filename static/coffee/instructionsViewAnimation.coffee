instructionsViewAnimation = (dimensions, getInstructionLocation, getBufferLocations, 
                             getBufferCells, getInstructionAt) ->
    # instruction -> img
    instructions = {}
    # [img]
    opponentInstructions = []
    # buffer -> img
    buffers = {}
    canvas = undefined

    initialize = (c) ->
        canvas = c

    registerInstruction = (img) ->
        instructions[img.instruction] = img

    registerBuffer = (img) ->
        buffers[img.buffer] = img

    timeout = (delay, f) ->
        window.setTimeout f, delay

    startAnimation = (callback) ->
        moveBuffers ->
            hideInstructions ->
                showOpponentBuffers ->
                    callback()

    hideInstructions = (callback) ->
        for instruction, img of instructions
            img.set {visible:false}
        callback()
        return

    moveBuffers = (callback) ->
        duration = 500
        animateAll callback, getBufferLocations(), (buffer, coords, cb) ->
            [x,y] = coords
            if not buffers[buffer].opponent
                buffers[buffer].animate {left:x}, {
                    onChange: canvas.renderAll.bind(canvas)
                    duration: duration
                    onComplete: cb
                }
                for i in [0..3]
                    ins = getInstructionAt buffer, i
                    if ins
                        instructions[ins].animate {left: x}, {
                            onChange: canvas.renderAll.bind(canvas)
                            duration: duration
                        }
            else
                cb()
        return

    showOpponentBuffers = (callback) ->
        for buffer, [x,y] of getBufferLocations()
            img = buffers[buffer]
            if img.opponent
                img.set {
                    left: x
                    top: y
                    visible: true
                }
        callback()

    animateMove = (buffer, sequence, instruction, callback) ->
        ins = getInstructionAt buffer, sequence
        if ins
            # our move
            instructions[ins].set {
                visible: true
                selectable: false
            }
        else
            # opponent's move
            [x,y] = getBufferCells()[buffer][sequence]
            fabric.Image.fromURL "/static/img/instructions/#{instruction}.svg", (img) ->
                img.set {
                    left: x
                    top: y
                    width: dimensions.instructionWidth
                    height: dimensions.instructionHeight
                    selectable: false
                    hasControls: false
                    hasBorders: false
                }
                opponentInstructions.push img
                canvas.add img
        canvas.renderAll()
        callback()
        return

    # places an instruction in a buffer cell
    placeInstruction = (instruction, buffer, sequence) ->
        [x, y] = getBufferCells()[buffer][sequence]
        img = instructions[instruction]
        img.set {left: x, top: y}
        img.bringToFront()
        canvas.renderAll()
        return


    endAnimation = (callback) ->
        hideOpponentBuffers ->
            moveBuffers ->
                resetInstructions ->
                    showHiddenInstructions ->
                        callback()
        return

    hideOpponentBuffers = (callback) ->
        for img in opponentInstructions
            img.remove()
        for buffer, img of buffers
            if img.opponent
                img.set {visible: false}
        callback()
        return

    resetInstructions = (callback) ->
        duration = 300
        animateAll callback, buffers, (buffer, _, cb) ->
            for i in [0..3]
                instruction = getInstructionAt buffer, i
                if instruction
                    img = instructions[instruction]
                    [x,y] = getInstructionLocation instruction
                    img.set {
                        visible: true
                        selectable: true
                    }
                    img.animate {left: x, top: y}, {
                        duration: duration,
                        onChange: canvas.renderAll.bind(canvas)
                        onComplete: cb
                    }
                else
                    cb()
        return


    showHiddenInstructions = (callback) ->
        for instruction, img of instructions
            img.set {visible: true}
        canvas.renderAll()
        callback()
        return


    return {initialize, registerInstruction, registerBuffer, placeInstruction,
            startAnimation, animateMove, endAnimation}
