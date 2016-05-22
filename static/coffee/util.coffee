animateAll = (callback, object, func) ->
    remaining = Object.keys(object).length
    if remaining == 0
        callback()
    else
        for k, v of object
            func k, v, ->
                remaining -= 1
                if remaining == 0
                    callback()
