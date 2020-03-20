"use strict";

exports.alert = function(msg) {
    return function() {
        window.alert(msg);
    };
};

exports.alertUncurried = function(msg) {
    window.alert(msg);
};