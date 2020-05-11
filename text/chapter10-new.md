# The Foreign Function Interface

## Chapter Goals

This chapter will introduce PureScript's _foreign function interface_ (or _FFI_), which enables communication from PureScript code to JavaScript code, and vice versa. We will cover how to:

- Call pure and effectful JavaScript functions from PureScript.
- Call PureScript functions from JavaScript.
- Understand the representation of PureScript values at runtime.
- Work with untyped data using the `foreign` package.
- Encode and parse JSON

Towards the end of this chapter, we will revisit our recurring address book example. The goal of the chapter will be to add the following new functionality to our application using the FFI:

- Alert the user with a popup notification.
- Store the serialized form data in the browser's local storage, and reload it when the application restarts.

## Project Setup

The source code for this module is a continuation of the source code from chapters 3, 7 and 8. As such, the source tree includes the appropriate source files from those chapters.

This chapter intruduces the `foreign-generic` library as a dependency. This library adds support for _datatype generic programming_ to the `foreign` library. The `foreign` library is a sub-dependency and provides a data type and functions for working with _untyped data_.

## A Disclaimer

PureScript provides a straightforward foreign function interface to make working with JavaScript as simple as possible. However, it should be noted that the FFI is an _advanced_ feature of the language. To use it safely and effectively, you should have an understanding of the runtime representation of the data you plan to work with. This chapter aims to impart such an understanding as pertains to code in PureScript's standard libraries.

PureScript's FFI is designed to be very flexible. In practice, this means that developers have a choice, between giving their foreign functions very simple types, or using the type system to protect against accidental misuses of foreign code. Code in the standard libraries tends to favor the latter approach.

As a simple example, a JavaScript function makes no guarantees that its return value will not be `null`. Indeed, idiomatic JavaScript code returns `null` quite frequently! However, PureScript's types are usually not inhabited by a null value. Therefore, it is the responsibility of the developer to handle these corner cases appropriately when designing their interfaces to JavaScript code using the FFI.

## Calling JavaScript From PureScript

The simplest way to use JavaScript code from PureScript is to give a type to an existing JavaScript value using a _foreign import_ declaration. Foreign import declarations should have a corresponding JavaScript declaration in a _foreign JavaScript module_.

For example, consider the `encodeURIComponent` function, which can be used from JavaScript to encode a component of a URI by escaping special characters:

```text
$ node

node> encodeURIComponent('Hello World')
'Hello%20World'
```

This function has the correct runtime representation for the function type `String -> String`, since it takes non-null strings to non-null strings, and has no other side-effects.

We can assign this type to the function with the following foreign import declaration:

```haskell
module Data.URI where

foreign import encodeURIComponent :: String -> String
```

We also need to write a foreign JavaScript module. If the module above is saved as `src/Data/URI.purs`, then the foreign JavaScript module should be saved as
`src/Data/URI.js`:

```javascript
"use strict";

exports.encodeURIComponent = encodeURIComponent;
```

Spago will find `.js` files in the `src` directory, and provide them to the compiler as foreign JavaScript modules.

JavaScript functions and values are exported from foreign JavaScript modules by assigning them to the `exports` object just like a regular CommonJS module. The `purs` compiler treats this module like a regular CommonJS module, and simply adds it as a dependency to the compiled PureScript module. However, when bundling code for the browser with `psc-bundle` or `spago bundle-app --to`, it is very important to follow the pattern above, assigning exports to the `exports` object using a property assignment. This is because `psc-bundle` recognizes this format,
allowing unused JavaScript exports to be removed from bundled code.

With these two pieces in place, we can now use the `encodeURIComponent` function from PureScript like any function written in PureScript. For example, if this declaration is saved as a module and loaded into PSCi, we can reproduce the calculation above:

```text
$ spago repl

> import Data.URI
> encodeURIComponent "Hello World"
"Hello%20World"
```

We can also define our own functions in foreign modules. Here's an example of how to create and call a custom JavaScript function that squares a `Number`:

`src/Test/Calculate.js`:
```js
"use strict";

exports.square = function(n) {
  return n * n;
};
```

`src/Test/Calculate.purs`:
```hs
module Test.Calculate where

foreign import square :: Number -> Number
```

```text
$ spago repl

> import Test.Calculate
> square 5.0
25.0
```

## Functions of Multiple Arguments

Let's rewrite our `diagonal` function from Chapter 2 in a foreign module to demonstrate how to call functions of multiple arguments. Recall that this function calculates the diagonal of a right-angled triangle:

```js
exports.diagonal = function(w, h) {
  return Math.sqrt(w * w + h * h);
};
```

Because PureScript uses curried functions of *single arguments*, we cannot directly import the `diagonal` function of *two arguments* like so:
```hs
-- This will not work with above uncurried definition of diagonal
foreign import diagonal :: Number -> Number -> Number
```

However, there are a few solutions to this dilemma:

The first option is to import and run the function with an `Fn` wrapper from `Data.Function.Uncurried` (`Fn` and uncurried functions are discussed in more detail later):
```hs
foreign import diagonal :: Fn2 Number Number Number
```

```text
$ spago repl

> import Test.Calculate
> import Data.Function.Uncurried
> runFn2 diagonal 3.0 4.0
5.0
```

The second option is to wrap or rewrite the function as curried JavaScript:

```js
exports.diagonalNested = function(w) {
  return function (h) {
    return Math.sqrt(w * w + h * h);
  };
};
```

or equivalently with arrow functions (see ES6 note below):

```js
exports.diagonalArrow = w => h =>
  Math.sqrt(w * w + h * h);
```

```hs
foreign import diagonalNested :: Number -> Number -> Number
foreign import diagonalArrow  :: Number -> Number -> Number
```

```text
$ spago repl

> import Test.Calculate
> diagonalNested 3.0 4.0
5.0
> diagonalArrow 3.0 4.0
5.0
```

## A Note About Uncurried Functions

PureScript's Prelude contains an interesting set of examples of foreign types. As we have covered already, PureScript's function types only take a single argument, and can be used to simulate functions of multiple arguments via _currying_. This has certain advantages - we can partially apply functions, and give type class instances for function types - but it comes with a performance penalty. For performance critical code, it is sometimes necessary to define genuine JavaScript functions which accept multiple arguments. The Prelude defines foreign types which allow us to work safely with such functions.

For example, the following foreign type declaration is taken from the Prelude in the `Data.Function.Uncurried` module:

```haskell
foreign import data Fn2 :: Type -> Type -> Type -> Type
```

This defines the type constructor `Fn2` which takes three type arguments. `Fn2 a b c` is a type representing JavaScript functions of two arguments of types `a` and `b`, and with return type `c`.

The `functions` package defines similar type constructors for function arities from 0 to 10.

We can create a function of two arguments by using the `mkFn2` function, as follows:

```haskell
import Data.Function.Uncurried

uncurriedAdd :: Fn2 Int Int Int
uncurriedAdd = mkFn2 \n m -> m + n
```

and we can apply a function of two arguments by using the `runFn2` function:

```text
$ spago repl

> import Test.Calculate
> import Data.Function.Uncurried
> runFn2 uncurriedAdd 3 10
13
```

The key here is that the compiler _inlines_ the `mkFn2` and `runFn2` functions whenever they are fully applied. The result is that the generated code is very compact:

```javascript
var uncurriedAdd = function (n, m) {
  return m + n | 0;
};
```

For contrast, here is a traditional curried function:

```haskell
curriedAdd :: Int -> Int -> Int
curriedAdd n m = m + n
```

and the resulting generated code, which is less compact due to the nested functions:

```javascript
var curriedAdd = function (n) {
  return function (m) {
    return m + n | 0;
  };
};
```

## A Note About Modern JavaScript Syntax

The arrow function syntax we saw earlier is an ES6 feature, and so it is incompatible with some older browsers (namely IE11). As of writing, it is [estimated that arrow functions are unavailable for the 6% of users](https://caniuse.com/#feat=arrow-functions) who have not yet updated their web browser.

In order to be compatible with the most users, the JavaScript code generated by the PureScript compiler does not use arrow functions. It is also recommended to avoid arrow functions in public libraries for the same reason.

You may still use arrow functions in your own FFI code, but then should include a tool such as [Babel](https://github.com/babel/babel#intro) in your deployment workflow to convert these back to ES5 compatibile functions.

If you find arrow functions in ES6 more readable, you may transform JavaScript code in the compier's `output` directory with a tool like [Lebab](https://github.com/lebab/lebab):

```sh
npm i -g lebab
lebab --replace output/ --transform arrow,arrow-return
```

This operation would convert the above `curriedAdd` function to:
```js
var curriedAdd = n => m =>
  m + n | 0;
```

The remaining examples in this book will use arrow functions instead of nested functions.

## Exercises
1. (Medium) Write a JavaScript function `volumeFn` in the `Test.Calculate` module that finds the volume of a box. Use an `Fn` wrapper from `Data.Function.Uncurried`.
2. (Medium) Rewrite `volumeFn` with arrow functions as `volumeArrow`.

## Beyond Simple Types

We have seen examples of how to send and receive primitive types, such as `String` and `Number`, over FFI. Now we'll cover how to use some of the other types available in PureScript, like `Maybe`.

Suppose we wanted to recreate the `head` function on arrays by using a foreign declaration. In JavaScript, we might write the function as follows:

```javascript
exports.head = arr =>
  arr[0];
```

However, there is a problem with this function. We might try to give it the type `forall a. Array a -> a`, but for empty arrays, this function returns `undefined`. Therefore, this function does not have the correct runtime representation.

We can instead return a `Maybe` value to handle this corner case.

It is tempting to write the following:
```js
exports.maybeHead = arr => {
  if (arr.length) {
    return Data_Maybe.Just.create(arr[0]);
  } else {
    return Data_Maybe.Nothing.value;
  }
}
```

```hs
foreign import maybeHead :: forall a. Array a -> Maybe a
```

But calling these `Maybe` constructors directly in the FFI code isn't recommended as it makes the code brittle to changes in the code generator. Additionally, doing this can cause problems when using `purs bundle` for dead code elimination.

The recommended approach is to add extra parameters to your FFI-defined function to accept the functions you need to call as arguments:

```js
exports.maybeHeadImpl = just => nothing => arr => {
  if (arr.length) {
    return just(arr[0]);
  } else {
    return nothing;
  }
};
```

```hs
foreign import maybeHeadImpl :: forall a. (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Array a -> Maybe a

maybeHead :: forall a. Array a -> Maybe a
maybeHead arr = maybeHeadImpl Just Nothing arr
```

Note that we wrote:
```hs
forall a. (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Array a -> Maybe a
```
and not:
```hs
forall a. ( a -> Maybe a) -> Maybe a -> Array a -> Maybe a
```

While both forms work, the latter is more vulnerable to unwanted inputs in place of `Just` and `Nothing`.
For example, in the more vulnerable case we could call it as follows:
```hs
maybeHeadImpl (\_ -> Just 1000) (Just 1000) [1,2,3]
```
which returns `Just 1000` for any array input.
This vulnerability is allowed because `(\_ -> Just 1000)` and `Just 1000` match the signatures of `(a -> Maybe a)` and `Maybe a` respectively when `a` is `Int` (based on input array).

In the more secure type signature, even when `a` is determined to be `Int` based on the input array, we still need to provide valid functions matching the signatures involving `forall x`.
The *only* option for `(forall x. Maybe x)` is `Nothing`, since a `Just` would assume a type for `x` and then no longer be valid for all `x`. The only options for `(forall x. x -> Maybe x)` are `Just` (our desired argument) and `(\_ -> Nothing)`, which is the only remaining vulnerability.

Todo - Questioning whether the above distinction is necessary. The vulnerability is really just if there's a mistake in the adjacent wrapper, and the suggested approach doesn't eliminate all vulnerabilities. The suggested approach can't be applied to most other wrapping strategies, either.

## Defining Foreign Types

Suppose instead of returning a `Maybe a`, we wanted to return a new type `Undefined a` whose representation at runtime was like that for the type `a`, but also allowing the `undefined` value.

We can define a _foreign type_ using the FFI using a _foreign type declaration_. The syntax is similar to defining a foreign function:

```haskell
foreign import data Undefined :: Type -> Type
```

Note that the `data` keyword here indicates that we are defining a type, not a value. Instead of a type signature, we give the _kind_ of the new type. In this case, we declare the kind of `Undefined` to be `Type -> Type`. In other words, `Undefined` is a type constructor.

We can now simply reuse our original definition for `head`:

```javascript
exports.undefinedHead = arr =>
  arr[0];
```

And in the PureScript module:

```haskell
foreign import undefinedHead :: forall a. Array a -> Undefined a
```

The body of the `undefinedHead` function returns `arr[0]` even if that value is undefined, and the type signature reflects the fact that our function can return an undefined value.

This function has the correct runtime representation for its type, but is quite useless since we have no way to use a value of type `Undefined a`. But we can fix that by writing some new functions using the FFI!

The most basic function we need will tell us whether a value is defined or not:

```haskell
foreign import isUndefined :: forall a. Undefined a -> Boolean
```

This is easily defined in our foreign JavaScript module as follows:

```javascript
exports.isUndefined = value =>
  value === undefined;
```

We can now use `isUndefined` and `undefinedHead` together from PureScript to define a useful function:

```haskell
isEmpty :: forall a. Array a -> Boolean
isEmpty = isUndefined <<< undefinedHead
```

Here, the foreign function we defined is very simple, which means we can benefit from the use of PureScript's typechecker as much as possible. This is good practice in general: foreign functions should be kept as small as possible, and application logic moved into PureScript code wherever possible.

## Exceptions

Another option is to simply throw an exception in the case of an empty array. Strictly speaking, pure functions should not throw exceptions, but we have the flexibility to do so. We indicate the lack of safety in the function name:

```haskell
foreign import unsafeHead :: forall a. Array a -> a
```

In our foreign JavaScript module, we can define `unsafeHead` as follows:

```javascript
exports.unsafeHead = arr => {
  if (arr.length) {
    return arr[0];
  } else {
    throw new Error('unsafeHead: empty array');
  }
};
```


## Exercises
1. (Hard) Write a JavaScript function `quadraticRootsImpl` and a wrapper `quadraticRoots :: Number -> Number -> Number -> Pair (Complex Number)` in the `Test.Calculate` module that uses the quadratic formula to find the roots of the polynomial `a*x^2 + b*x + c` when provided with the coefficients `a`, `b`, `c`. Return the two roots as a `Pair` of `Complex` `Number`s. *Hint:* Use the `quadraticRoots` wrapper to pass constructors for `Pair` and `Complex` to `quadraticRootsImpl`.

Todo - sort pair for test

## Using Type Class Member Functions

Just like our earlier guide on passing the `Maybe` constructor over FFI, this is another case of writing PureScript that calls JavaScript which in turn calls PureScript functions again. Here we will explore how to pass type class member functions over the FFI.

Todo - these should be in `Test.Display`.

Here is one way to give JavaScript access PureScript's `show` so that the foreign code has a way to generate a `String` from any `Show`able types.

We start with writing a foreign JavaScript function which expects the appropriate instance of `show` to match the type of `x`.

```js
exports.boldImpl = show => x =>
  show(x).toUpperCase() + "!!!";
```

Then we write the matching signature:
```hs
foreign import boldImpl :: forall a. (a -> String) -> a -> String
```

and a wrapper function that passes the correct instance of `show`:
```hs
boldWrap :: forall a. Show a => a -> String
boldWrap x = boldImpl show x
```

We can then call the wrapper:
```text
$ spago repl

> import Test.Display
> import Data.Tuple
> boldWrap (Tuple 1 "Hat")
"(TUPLE 1 \"HAT\")!!!"
```

Another option is to include the type class constraint (`Show a =>`) in the foreign signature, rather than only in the wrapping function.

```hs
foreign import boldConstraint :: forall a. Show a => a -> String
```

This type class constraint results in another argument (`Show` in this case) passed to the corresponding JavaScript function:
```js
exports.boldConstraint = Show => x =>
  Show.show(x).toUpperCase() + "!!!";
```

The `Show` parameter is a javascript object that contains all of the type class member functions for the `Show` constraint. Each member function (just `show` in this case), is specialized for the constrained type `a`. In other words, the compiler will select the functions defined in the type class instance of the type that `a` happens to be.

We can then avoid a wrapper and call the foreign function directly:
```text
$ spago repl

> import Test.Display
> import Data.Tuple
> boldConstraint (Tuple 1 "Hat")
"(TUPLE 1 \"HAT\")!!!"
```

Todo - is this approach more likely to break with dead-code elimination? If a type class constraint is declared, will member functions for the discovered types be preserved, or must these member functions actually be used in the function?

Here's another example demonstrating multiple constraints:

```js
exports.showEquality = Eq => Show => a => b => {
  if (Eq.eq(a)(b)) {
    return "Equivalent";
  } else {
    return Show.show(a) + " is not equal to " + Show.show(b);
  }
}
```

```hs
foreign import showEquality :: forall a. Eq a => Show a => a -> a -> String
```

```text
$ spago repl

> import Test.Display
> import Data.Maybe
> showEquality Nothing (Just 5)
"Nothing is not equal to (Just 5)"
```

## Effectful Functions

Let's extend our `boldConstraint` function to log to the console. Logging is an `Effect` and `Effect`s are represented in JavaScript as a function of zero arguments, `()` with arrow notation:

```js
exports.yell = Show => x => () =>
  console.log(Show.show(x).toUpperCase() + "!!!");
```

The new foreign import is the same as before, except that the return type changed from `String` to `Effect Unit`.
```hs
foreign import yell :: forall a. Show a => a -> Effect Unit
```

When testing this in the repl, notice that string is printed directly to the console (instead of being quoted) and a `unit` value is returned.

```text
$ spago repl

> import Test.Display
> import Data.Tuple
> yell (Tuple 1 "Hat")
(TUPLE 1 "HAT")!!!
unit
```

There are also `EffectFn` wrappers from `Effect.Uncurried`. These are similar to the similar to the `Fn` wrappers from `Data.Function.Uncurried` that we've already seen. These wrappers let you call uncurried effectful functions in PureScript.

You'd generally only use these if you want to call existing JavaScript library APIs directly, rather than wrapping those APIs in curried functions. So it doesn't make much sense to present an example of uncurried `yell` where the JavaScript relies on PureScript type class members, since you wouldn't find that in the existing JavaScript ecosystem.

Todo - is it even possible to grab constraint args with runFn? Maybe this should be a note in the Constraint section.

Instead, we'll modify our previous `diagonal` example to include logging in addition to returning the result:

```js
exports.diagonalLog = function(w, h) {
  let result = Math.sqrt(w * w + h * h);
  console.log("Diagonal is " + result);
  return result;
};
```

```hs
foreign import diagonalLog :: EffectFn2 Number Number Number
```

```text
$ spago repl

> import Test.Calculate
> import Effect.Uncurried
> runEffectFn2 diagonalLog 3.0 4.0
Diagonal is 5
5.0
```

## Asynchronous Functions

Promises in JavaScript translate directly to asynchronous effects in PureScript with the help of the `Control.Promise` module.

Take this `sleep` `Promise` as an example:
```js
exports.sleep = ms =>
  new Promise(resolve => setTimeout(resolve, ms));
```

It delays execution for `ms` milliseconds, and is represented by this PureScript signature:
```hs
foreign import sleep :: Int -> Promise Unit
```

We can then convert this `Promise` to an `Aff` with `toAff`.

```text
$ spago repl

> import Prelude
> import Test.Calculate
> import Control.Promise
> import Effect.Class.Console
> import Effect.Aff
> :pa
… launchAff_ do
…   log "waiting"
…   toAff $ sleep 300
…   log "done waiting"

waiting
unit
done waiting
```

Note that asynchronous logging in the repl just waits to print until the entire block has finished executing. This code behaves more predictably when run with `spago test` where there is a slight delay *between* prints.

PureScript's `Promise` type also represents `async`/`await` functions, which are just syntactic sugar for promises. Here we use the `async` keyword for the last parameter:

```js
exports.diagonalAsync = w => async h => {
  await exports.sleep(300);
  return Math.sqrt(w * w + h * h);
};
```

Since we're returning a `Number` we represent this type in the `Promise`:

```hs
foreign import diagonalAsync :: Number -> Number -> Promise Number
```

```text
$ spago repl

import Prelude
import Test.Calculate
import Control.Promise
import Effect.Class.Console
import Effect.Aff
> :pa
… launchAff_ do
…   res <- toAff $ diagonalAsync 3.0 4.0
…   logShow res
…
unit
5.0
```

We can also wrap `Promise` with `Effect`:

```hs
foreign import diagonalAsyncEffect :: Number -> Number -> Effect (Promise Number)
```

The corresponding JavaScript needs to return a function of zero arguments to represent the `Effect`:

```js
exports.diagonalAsyncEffect = w => h => async function() {
  await exports.sleep(300);
  return Math.sqrt(w * w + h * h);
};
```

And `toAffE` is used instead of `toAff` to convert to an `Aff`:
```text
$ spago repl

import Prelude
import Test.Calculate
import Control.Promise
import Effect.Class.Console
import Effect.Aff
> :pa
… launchAff_ do
…   res <- toAffE $ diagonalAsyncEffect 3.0 4.0
…   logShow res
…
unit
5.0
```

Todo - why doesn't arrow syntax work with async effect?

## Passing JSON
## Simple JSON

Just do a really simple example.

Map with pair to string?

Note all of these convert to the same JSON representation:
Set, Array, Tuple, Pair

"To encode JSON, you must decide on a way to represent your data using only primitive JSON types (strings, numbers, booleans, arrays, objects, or null).


Should probably just do a send two complex records, then receive a complex record.


Bonus challenge could be to receive a record with coefficients. Actually, can't go the other way.

Could do something with a set or map.



Combined. Just use Simple JSON, but think of some good examples.
Actually, Argonaut seems to work better.

What to pass to JS?

What to get from JS?

Map? Set? Pair of Complex?

rewrite quadratic which takes a record of coefficients, a, b, c.
Can you just pass a record?  Or must it be represented as JSON?


Todo - SimpleJSON docs need some minor updates.

## Calling PureScript from JavaScript
runtime representations too.
Move these to end - What should this section be called:
Additional Content, Addendum?

## Address book

