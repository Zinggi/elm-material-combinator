const _user$project$Native_Reflection = (() => {

const getAccessorName = (f, string) => {
    var handler = {
        get: function(target, name) {
            return name;
        }
    };
    // we use a Proxy here to find out which property is trying to be accessed.
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy
    var p = new Proxy({}, handler);
    return f(p);
};

return {
    getAccessorName: getAccessorName
};

})();
