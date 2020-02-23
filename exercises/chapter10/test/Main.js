"use strict";

exports.foreignLog = function(msg) {
    return function() {
        console.log(msg);
    };
};
