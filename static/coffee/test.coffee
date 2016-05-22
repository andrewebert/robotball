test = (inst, process) ->
    inst.placeInstruction("North1", "Red", 0)
    inst.placeInstruction("East2", "Red", 1)
    actions = JSON.parse '[{"value":{"card":"South3","color":"Blue"},"key":"play"},{"value":{"Red":{"direction":"North","layer":0,"position":{"x":3,"y":0}},"Blue":{"direction":"South","layer":0,"position":{"x":3,"y":11}}},"key":"update"},{"value":{"Blue":{"direction":"South","layer":0,"position":{"x":3,"y":10}}},"key":"update"},{"value":{"Blue":{"direction":"South","layer":0,"position":{"x":3,"y":9}}},"key":"update"},{"value":{"card":"North1","color":"Red"},"key":"play"},{"value":{"Red":{"direction":"North","layer":0,"position":{"x":3,"y":1}}},"key":"update"},{"value":{"card":"East1","color":"Blue"},"key":"play"},{"value":{"Blue":{"direction":"East","layer":0,"position":{"x":3,"y":9}}},"key":"update"},{"value":{"Blue":{"direction":"East","layer":0,"position":{"x":4,"y":9}}},"key":"update"},{"value":{"card":"East2","color":"Red"},"key":"play"},{"value":{"Red":{"direction":"East","layer":0,"position":{"x":3,"y":1}}},"key":"update"},{"value":{"Red":{"direction":"East","layer":0,"position":{"x":4,"y":1}}},"key":"update"},{"value":{"Red":{"direction":"East","layer":0,"position":{"x":5,"y":1}}},"key":"update"},{"value":"Red","key":"startingPlayer"}]'
    process(actions)
    return