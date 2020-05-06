"use strict";

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
