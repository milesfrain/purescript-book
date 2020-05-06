"use strict";

var Tg = require('../Test.Gcd');
var Ts = require('../Test.Shout');

exports.runGcd = function(n) {
  return function(m) {
    return Tg.gcd(n)(m);
  };
};

exports.runShout = Ts.shout(require('../Data.Show').showNumber)(42);


exports.boldImpl = show => x =>
  show(x).toUpperCase() + "!!!";

exports.boldConstraint = Show => x =>
  Show.show(x).toUpperCase() + "!!!";

exports.showEquality = Eq => Show => a => b => {
  if (Eq.eq(a)(b)) {
    return "Equivalent";
  } else {
    return Show.show(a) + " is not equal to " + Show.show(b);
  }
}

exports.yell = Show => x => () =>
  console.log(Show.show(x).toUpperCase() + "!!!");

//exports.hasEvenImpl = prod => xs => prod(xs) % 2 == 0;



/*
Problematic because product is not a direct instance function of Foldable.
*/
/*
exports.hasEvenConstraint = Fld => xs => {
  console.log(Fld)
  return true
  //Fld.product(xs) % 2 == 0;
}
 */