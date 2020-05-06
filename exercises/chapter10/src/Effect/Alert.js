"use strict";

exports.alert = function(msg) {
    return function() {
        //window.alert(msg);
        console.log("V4 " + msg);
    };
};
