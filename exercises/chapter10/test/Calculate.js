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


exports.cumulativeSums = arr => {
  let sum = 0
  let sums = []
  arr.forEach(x => {
    sum += x;
    sums.push(sum);
  });
  return sums;
};


exports.addComplex = a => b => {
  return {
    real: a.real + b.real,
    imag: a.imag + b.imag
  }
};

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


exports.quadraticRootsImpl = mkPair => a => b => c =>
  mkPair({real:1, imag:2})({real:3, imag:4});


exports.myArr = arr => [1, 2, 3.2, 'c']


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


exports.cumulativeSumsBroken = arr => {
  let sum = 0
  let sums = []
  arr.forEach(x => {
    sum += x;
    sums.push(sum);
  });
  sums.push("Broken"); // Bug
  return sums;
};


exports.addComplexBroken = a => b => {
  return {
    real: a.real + b.real,
    broken: a.imag + b.imag // Bug
  }
};

exports.cumulativeSumsJson = exports.cumulativeSumsBroken
// Try the non-broken version too
//exports.cumulativeSumsJson = exports.cumulativeSums

exports.addComplexJson = exports.addComplexBroken
// Try the non-broken version too
//exports.addComplexJson = exports.addComplex

exports.mapSetFooJson = j => {
  let m = new Map(j);
  m.set("Foo", 42);
  return Array.from(m);
};

/*
exports.mapSetFooJson = m =>
  Array.from((new Map(m)).set("Foo", 42));
*/

exports.valuesOfMapJson = j => {
  let m = new Map(j);
  let s = new Set(m.values())
  return Array.from(s);
};