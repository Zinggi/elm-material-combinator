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
    var name = f(p);
    if (Object.prototype.toString.call(name) === "[object String]") { // if name is a string
        return _elm_lang$core$Result$Ok(name);
    } else {
        return _elm_lang$core$Result$Err("Not an accessor function!");
    }
};

return {
    getAccessorName: getAccessorName
};

})();
