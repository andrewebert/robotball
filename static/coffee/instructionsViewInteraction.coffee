instructionsViewInteration = (dimensions, getInstructionLocation, getBufferCells, \
                              getInstructionAt, setInstructionAt) ->
    addInteraction = (img, canvas) ->
        img.on 'modified',  ->
            snap img, canvas
        img.on 'moving', ->
            img.bringToFront()
        return


    snap = (img, canvas) ->
        setInstructionAt img.instruction, undefined, undefined
        grid = findGrid img.instruction, img.left, img.top

        if grid.buffer and not getInstructionAt grid.buffer, grid.sequence
            setInstructionAt img.instruction, grid.buffer, grid.sequence
            [x,y] = [grid.x, grid.y]
        else
            [x,y] = getInstructionLocation img.instruction

        img.animate {left: x, top: y}, {
            onChange: canvas.renderAll.bind(canvas)
            duration: 100
        }
        return


    findGrid = (instruction, left, top) ->
        height = dimensions.instructionBoxHeight
        width = dimensions.instructionWidth
        centerX = left + width/2
        centerY = top + height/2

        for buffer, cells of getBufferCells()
            for [boxLeft, boxTop], i in cells
                boxRight = boxLeft + width
                boxBottom = boxTop + height
                if centerX >= boxLeft && centerX < boxRight \
                        && centerY >= boxTop && centerY < boxBottom
                    return {
                        buffer: buffer
                        sequence: i
                        x: boxLeft
                        y: boxTop
                    }
        return {
            buffer: undefined
            sequence: undefined
            x: undefined
            y: undefined
        }

    return {addInteraction: addInteraction}
