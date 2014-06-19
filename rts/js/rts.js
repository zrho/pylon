function thunk(f) {
    return function(obj) {
        var fe = f();
        obj.index = fe.index;
        obj.arity = fe.arity;
        obj.type  = fe.type;
        obj.code  = fe.code;
        obj.data  = fe.data;
        return obj;
    }
}

function force(f) {
    while (f.type == 3) f = f.code(f);
    return f;
}

function apply() {
    var args = Array.prototype.slice.call(arguments); // todo: wut?
    while (1) {
        var f    = force(args[0]);
        var argc = args.length - 1;

        switch (f.type) {
            case 0: // Fun
                if (argc == f.arity) {
                    // saturated
                    return f.code.apply(f, args.slice(1, argc + 1));
                } else if (argc > f.arity) {
                    // oversaturated
                    var g = f.code.apply(f, args.slice(1, f.arity));
                    args  = [g].concat(args.slice(f.arity + 1, argc + 1));
                    //tailrec
                } else {
                    // undersaturated
                    return {
                        type: 1,
                        code: f,
                        data: args.slice(1, argc + 1)
                    };
                }
                break;
            case 1: // Pap
                args = [f.code].concat(f.data).concat(args.slice(1, argc + 1));
                //tailrec
                break;
        }
    }
}

function deepforce(f) {
    f = force(f);
    if (f.type == 2) {
        for (var i = 0; i < f.data.length; ++i) {
            f.data[i] = deepforce(f.data[i]);
        }
    }
    return f;
}

module.exports = {
    thunk: thunk,
    force: force,
    apply: apply,
    deepforce: deepforce
}