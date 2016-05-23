# returns initialize :: (img -> canvas -> ()) -> canvas
instructionsViewSetup = (dimensions, instructions, opponentBufferLabels, 
                         getInstructionLocations, getBufferLocations) ->
    initialize = (instructionCallback, bufferCallback, placeholderCallback) ->
        canvas = new fabric.Canvas 'instructions', {
            width: 480
            height: 810
            selection: false
        }
        
        placeInstructions canvas, instructionCallback
        placeBuffers canvas, bufferCallback
        placePlaceholders canvas, placeholderCallback

        return canvas


    # Places each instruction at its starting location, then calls a callback
    # for each Fabric object representing an instruction
    placeInstructions = (canvas, callback) ->
        placeInstruction = (instruction, x, y) ->
            fabric.Image.fromURL "/static/img/instructions/#{instruction}.png", (img) ->
                img.set {
                    left: x
                    top: y
                    width: dimensions.instructionWidth
                    height: dimensions.instructionHeight
                    selectable: true
                    hasControls: false
                    hasBorders: false
                    instruction: instruction
                }
                callback img, canvas
                canvas.add img

        for instruction, [x,y] of getInstructionLocations()
            placeInstruction instruction, x, y
        return


    # Places each buffer at its starting location, then calls a callback for each
    # Fabric object representing a buffer
    placeBuffers = (canvas, callback) ->
        placeBuffer = (buffer, x, y) ->
            fabric.Image.fromURL "/static/img/buffers/#{buffer}.png", (img) ->
                img.set {
                    left: x
                    top: y
                    width: dimensions.bufferWidth
                    height: dimensions.bufferHeight
                    selectable: false
                    buffer: buffer
                    opponent: false
                }
                callback img, canvas
                canvas.add img

        placeHiddenBuffer = (buffer) ->
            fabric.Image.fromURL "/static/img/buffers/#{buffer}.png", (img) ->
                img.set {
                    width: dimensions.bufferWidth
                    height: dimensions.bufferHeight
                    selectable: false
                    visible: false
                    buffer: buffer
                    opponent: true
                }
                callback img, canvas
                canvas.add img

        for buffer, [x,y] of getBufferLocations()
            placeBuffer buffer, x, y
        for buffer in opponentBufferLabels
            placeHiddenBuffer buffer

    placePlaceholders = (canvas, callback) ->
        placePlaceholder = (buffer) ->
            fabric.Image.fromURL "/static/img/robots/#{buffer}.png", (img) ->
                img.set {
                    width: dimensions.placeholderWidth
                    height: dimensions.placeholderHeight
                    selectable: false
                    visible: false
                }
                callback buffer, img
                canvas.add img
        for buffer in opponentBufferLabels
            placePlaceholder buffer

    return {
        initialize: initialize
    }

