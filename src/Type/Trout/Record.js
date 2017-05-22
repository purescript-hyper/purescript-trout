function shallowClone(r) {
    return Object.assign(Object.create(r), r);
}

exports.unsafeGet = function (l) {
    return function (r) {
        return r[l];
    };
};

exports.unsafeSet = function (l) {
    return function (x) {
        return function (r) {
            var clone = shallowClone(r);
            clone[l] = x;
            return clone;
        };
    };
};

exports.unsafeDelete = function (l) {
    return function (r) {
        var clone = shallowClone(r);
        delete clone[l];
        return clone;
    };
};
