"use strict";

exports.square = function(n) {
  return n * n;
};


exports.diagonal = function(w, h) {
  return Math.sqrt(w * w + h * h);
};

exports.diagonalNested = function(w) {
  return function (h) {
    return Math.sqrt(w * w + h * h);
  };
};

exports.diagonalArrow = w => h =>
  Math.sqrt(w * w + h * h);


exports.maybeHeadImpl = just => nothing => arr => {
  if (arr.length) {
    return just(arr[0]);
  } else {
    return nothing;
  }
};

exports.undefinedHead = arr =>
  arr[0];

exports.isUndefined = value =>
  value === undefined;

exports.unsafeHead = arr => {
  if (arr.length) {
    return arr[0];
  } else {
    throw new Error('unsafeHead: empty array');
  }
};


exports.diagonalLog = function(w, h) {
  let result = Math.sqrt(w * w + h * h);
  console.log("Diagonal is " + result);
  return result;
};


exports.sleep = ms =>
  new Promise(resolve => setTimeout(resolve, ms));

exports.diagonalAsync = w => async h => {
  await exports.sleep(300);
  return Math.sqrt(w * w + h * h);
};

/*
// Foreign syntax error with this for some reason
exports.diagonalAsyncEffect = w => h => async () => {
  await exports.sleep(300);
  return Math.sqrt(w * w + h * h);
};
*/

exports.diagonalAsyncEffect = w => h => async function() {
  await exports.sleep(300);
  return Math.sqrt(w * w + h * h);
};

exports.showQuadRec = r => {
  console.log(r);
  return 5;
};

exports.sh = x => {
  console.log(x);
  return 5;
};