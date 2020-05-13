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

This chapter introduces the `argonaut` library as a dependency. This library is used for encoding and decoding JSON.

Todo - Old
This chapter introduces the `foreign-generic` library as a dependency. This library adds support for _datatype generic programming_ to the `foreign` library. The `foreign` library is a sub-dependency and provides a data type and functions for working with _untyped data_.

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

You may still use arrow functions in your own FFI code, but then should include a tool such as [Babel](https://github.com/babel/babel#intro) in your deployment workflow to convert these back to ES5 compatible functions.

If you find arrow functions in ES6 more readable, you may transform JavaScript code in the compiler's `output` directory with a tool like [Lebab](https://github.com/lebab/lebab):

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

Todo - exercises should probably be written in a Test.Solutions module. We can add common types to a Test.Common module.

## Exercises
1. (Medium) Write a JavaScript function `volumeFn` in the `Test.Calculate` module that finds the volume of a box. Use an `Fn` wrapper from `Data.Function.Uncurried`.
2. (Medium) Rewrite `volumeFn` with arrow functions as `volumeArrow`.


## Passing Simple Types

The following data types may be passed between PureScript and JavaScript as-is:
* Boolean
* String
* Int / Number
* Array
* Record / Object

We've already seen examples with the primitive types `String` and `Number`. We'll now take a look at the structural types `Array` and `Record` (`Object` in JavaScript).

To demonstrate passing `Array`s, here's how to call a JavaScript function which takes an `Array` of `Int` and returns the cumulative sum as another array. Recall that since JavaScript does not have separate type for `Int`, both `Int` and `Number` in PureScript map to `Number` in JavaScript.

```hs
foreign import cumulativeSums :: Array Int -> Array Int
```

```js
exports.cumulativeSums = arr => {
  let sum = 0
  let sums = []
  arr.forEach(x => {
    sum += x;
    sums.push(sum);
  });
  return sums;
};
```

```text
$ spago repl

> import Test.Calculate
> cumulativeSums [1, 2, 3]
[1,3,6]
```

To demonstrate passing `Records`, here's how to call a JavaScript function which takes two `Complex` numbers as records, and returns their sum as another record. Note that a `Record` in PureScript is represented as an `Object` in JavaScript:

```hs
type Complex = {
  real :: Number,
  imag :: Number
}

foreign import addComplex :: Complex -> Complex -> Complex
```

```js
exports.addComplex = a => b => {
  return {
    real: a.real + b.real,
    imag: a.imag + b.imag
  }
};
```

```text
$ spago repl

> import Test.Calculate
> addComplex { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
{ imag: 6.0, real: 4.0 }
```

Note that the above techniques require trusting that JavaScript will return the expected types, as PureScript is not able to apply type checking to JavaScript code. We will describe this type safety concern in more detail later on in the JSON section, as well as cover techniques to protect against type mismatches.

## Exercises
1. (Medium) Write a JavaScript function `cumulativeSumsComplex` (and corresponding PureScript foreign import) that takes an `Array` of `Complex` numbers and returns the cumulative sum as another array of complex numbers.

## Beyond Simple Types

We have seen examples of how to send and receive types with a native JavaScript representation, such as `String`, `Number`, `Array`, and `Record`, over FFI. Now we'll cover how to use some of the other types available in PureScript, like `Maybe`.

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
1. (Medium) Given a record that represents a quadratic polynomial `a*x^2 + b*x + c`:
```hs
type Quadratic = {
  a :: Number,
  b :: Number,
  c :: Number
}
```
Write a JavaScript function `quadraticRootsImpl` and a wrapper `quadraticRoots :: Quadratic -> Pair Complex` in the `Test.Calculate` module that uses the quadratic formula to find the roots of this polynomial. Return the two roots as a `Pair` of `Complex` numbers. *Hint:* Use the `quadraticRoots` wrapper to pass a constructor for `Pair` to `quadraticRootsImpl`.

Todo - sort pair for test

## Using Type Class Member Functions

Just like our earlier guide on passing the `Maybe` constructor over FFI, this is another case of writing PureScript that calls JavaScript which in turn calls PureScript functions again. Here we will explore how to pass type class member functions over the FFI.

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

Todo - Exercises

## JSON

There are many reasons to use JSON in an application, for example, it's a common means of communicating with web APIs. This section will discuss other use-cases too, beginning with a technique to improve type safety when passing structural data over the FFI.

Let's revisit our earlier FFI functions `cumulativeSums` and `addComplex` and introduce a bug to each:

```js
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
```

We can use the original type signatures, and the code will still compile, despite the fact that the return types are incorrect.
```hs
foreign import cumulativeSumsBroken :: Array Int -> Array Int

foreign import addComplexBroken :: Complex -> Complex -> Complex
```

We can even execute the code, which might either produce unexpected results or a runtime error:
```text
$ spago repl

> import Test.Calculate
> import Data.Foldable (sum)

> sums = cumulativeSumsBroken [1, 2, 3]
> sums
[1,3,6,Broken]
> sum sums
0

> complex = addComplexBroken { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
> complex.real
4.0
> complex.imag + 1.0
NaN
> complex.imag
  var str = n.toString();
              ^
TypeError: Cannot read property 'toString' of undefined
```

For example, our resulting `sums` is no-longer a valid `Array Int`, now that a `String` is included in the Array. And further operations produce unexpected behavior, rather than an outright error, as the `sum` of these `sums` is `0` rather than `10`. This could be a difficult bug to track down!

Likewise, there are no errors when calling `addComplexBroken`; however, accessing the `imag` field of our `Complex` result will either produce unexpected behavior (returning `NaN` instead of `7.0`), or a non-obvious runtime error.

Let's use JSON to make our PureScript code more impervious to bugs in JavaScript code.

The `argonaut` library contains the JSON decoding and encoding capabilities we need. That library has excellent [documentation](https://github.com/purescript-contrib/purescript-argonaut#documentation), so we will only cover basic usage in this book.


If we create an alternate foreign import that defines the return type as `Json`:
```hs
foreign import cumulativeSumsJson :: Array Int -> Json
foreign import addComplexJson :: Complex -> Complex -> Json
```

Note that we're simply pointing to our existing broken functions:
```js
exports.cumulativeSumsJson = exports.cumulativeSumsBroken
exports.addComplexJson = exports.addComplexBroken
```

And then write a wrapper to decode the returned foreign `Json` value:
```hs
cumulativeSumsDecoded :: Array Int ->  Either String (Array Int)
cumulativeSumsDecoded arr = decodeJson $ cumulativeSumsJson arr

addComplexDecoded :: Complex -> Complex ->  Either String Complex
addComplexDecoded a b = decodeJson $ addComplexJson a b
```

Then any values that can't be successfully decoded to our return type appear as a `Left` error `String`:
```text
$ spago repl

> import Test.Calculate

> cumulativeSumsDecoded [1, 2, 3]
(Left "Couldn't decode Array (Failed at index 3): Value is not a Number")

> addComplexDecoded { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
(Left "JSON was missing expected field: imag")
```

If we call the working versions, a `Right` value is returned

```js
exports.cumulativeSumsJson = exports.cumulativeSums
exports.addComplexJson = exports.addComplex
```

```text
$ spago repl

> import Test.Calculate

> cumulativeSumsDecoded [1, 2, 3]
(Right [1,3,6])

> addComplexDecoded { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
(Right { imag: 6.0, real: 4.0 })
```

Using JSON is also the easiest way to pass other structural types, such as `Map` and `Set` through the FFI. Note that since JSON only consists of booleans, numbers, strings, and arrays and objects of other JSON values, we can't write a `Map` and `Set` directly in JSON, but we can represent these structures as arrays (assuming the keys and values can also be represented in JSON), and then decode them back to `Map` or `Set`.

Here's an example of a foreign function signature that modifies a `Map` of `String` keys and `Int` values, along with the wrapper function that handles JSON encoding and decoding.
```hs
foreign import mapSetFooJson :: Json -> Json

mapSetFoo :: Map String Int -> Either String (Map String Int)
mapSetFoo m = decodeJson $ mapSetFooJson $ encodeJson m
```

Here is the JavaScript implementation. Note the `Array.from` step which is necessary to convert the JavaScript `Map` into a JSON-friendly format before decoding converts it back to a PureScript `Map`.
```js
exports.mapSetFooJson = j => {
  let m = new Map(j);
  m.set("Foo", 42);
  return Array.from(m);
};
```

Now we can send and receive a `Map` over the FFI:
```text
$ spago repl

> import Test.Calculate
> import Data.Map
> import Data.Tuple

> myMap = fromFoldable [ Tuple "hat" 1, Tuple "cat" 2 ]

> :type myMap
Map String Int

> myMap
(fromFoldable [(Tuple "cat" 2),(Tuple "hat" 1)])

> mapSetFoo myMap
(Right (fromFoldable [(Tuple "Foo" 42),(Tuple "cat" 2),(Tuple "hat" 1)]))
```

## Exercises

1. (Medium) Write a JavaScript function and PureScript wrapper `valuesOfMap :: Map String Int -> Either String (Set Int)` that returns a `Set` of all the values in a `Map`. _Hint_: The `.values()` instance method for Map may be useful in your JavaScript code.
1. (Easy) Write a new wrapper for the previous JavaScript function with the signature `valuesOfMapGeneric :: forall k v. Map k v -> Either String (Set v)` so it works with a wider variety of maps. Note that you'll need to add some type class constraints for `k` and `v`. The compiler will guide you.
1. (Medium) Rewrite the earlier `quadraticRoots` function as `quadraticRootsSet` which returns the `Complex` roots as a `Set` via JSON (instead of as a `Pair`).  Todo - not sure if this exercise adds much since there's no need for writing a custom decoding/encoding instance for Complex. Was hoping that would be a good intermediate step before writing a custom instance (that requires newtype) in the next exercise.
1. (Hard) Rewrite the earlier `quadraticRoots` function as `quadraticRootsSafe` which uses JSON to pass the `Pair` of `Complex` roots over FFI (instead of using the `Pair` constructor in JavaScript).
_Hint_: You'll need to write a `DecodeJson` instance for `Pair`. Consult the [argonaut docs](https://github.com/purescript-contrib/purescript-argonaut-codecs#writing-new-instances) for instruction on writing your own decode instance. Their [decodeJsonTuple](https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/master/src/Data/Argonaut/Decode/Class.purs) instance may also be a helpful reference.  Note that you'll need a `newtype` wrapper for `Pair` to avoid creating an "orphan instance".

Todo - Verify these exercises work for argonaut

 1. (Easy) Use `decodeJSON` to parse a JSON document representing a two-dimensional JavaScript array of integers, such as `[[1, 2, 3], [4, 5], [6]]`. What if the elements are allowed to be null? What if the arrays themselves are allowed to be null?
 1. (Medium) Convince yourself that the implementation of `savedData` should type-check, and write down the inferred types of each subexpression in the computation.
 1. (Medium) The following data type represents a binary tree with values at the leaves:

     ```haskell
     data Tree a = Leaf a | Branch (Tree a) (Tree a)
     ```

     Derive `Encode` and `Decode` instances for this type using `foreign-generic`, and verify that encoded values can correctly be decoded in PSCi. Hint: This requires a [Generic Instance](https://github.com/paf31/24-days-of-purescript-2016/blob/master/11.markdown#deriving-generic-instances), also see previous section on "Instance Dependencies", and finally, search the web for "eta-expansion" if you encounter recursion issues during testing.
 1. (Difficult) The following `data` type should be represented directly in JSON as either an integer or a string:

     ```haskell
     data IntOrString
       = IntOrString_Int Int
       | IntOrString_String String
     ```

     Write instances for `Encode` and `Decode` for the `IntOrString` data type which implement this behavior, and verify that encoded values can correctly be decoded in PSCi.



Todo - discuss null values - already covered in argonaut docs.

## Address book

Todo - Address book should probably start with all chapter 8 exercises completed

In this section we will apply our newly-acquired FFI and JSON knowledge to build on our address book example from chapter 8. We will add the following features:
- A Save button at the bottom of the form that, when clicked, serializes the state of the form to JSON and saves it in local storage.
- Automatic retrieval of the JSON document from local storage upon page reload. The form fields are populated with the contents of this document.
- A pop-up alert if there is an issue saving or loading the form state.

We'll start by creating FFI wrappers for the following Web Storage APIs in our `Effect.Storage` module:
- `setItem` takes a key and a value (both strings), and returns a computation which stores (or updates) the value in local storage at the specified key.
- `getItem` takes a key, and attempts to retrieve the associated value from local storage. However, since the `getItem` method on `window.localStorage` can return `null`, the return type is not `String`, but `Json`.

```haskell
foreign import setItem :: String -> String -> Effect Unit

foreign import getItem :: String -> Effect Json
```

Here is the corresponding JavaScript implementation of these functions in `Effect/Storage.js`:

```js
exports.setItem = key => value => () =>
  window.localStorage.setItem(key, value);

exports.getItem = key => () =>
  window.localStorage.getItem(key);
```

We'll create a save button like so:
```hs
saveButton :: R.JSX
saveButton =
  D.label
    { className: "form-group row col-form-label"
    , children:
        [ D.button
            { className: "btn-primary btn"
            , onClick: handler_ validateAndSave
            , children: [ D.text "Save" ]
            }
        ]
    }
```

And write our validated `person` as a JSON string with `setItem` in the `validateAndSave` function:
```hs
validateAndSave :: Effect Unit
validateAndSave = do
  log "Running validators"
  case validatePerson' person of
    Left errs -> log $ "There are " <> show (length errs) <> " validation errors."
    Right validPerson -> do
      setItem "person" $ stringify $ encodeJson validPerson
      log "Saved"
```

Note that if we attempt to compile at this stage, we'll encounter the following error:
```text
  No type class instance was found for
    Data.Argonaut.Encode.Class.EncodeJson PhoneType
```

This is because `PhoneType` in the `Person` record needs an `EncodeJson` instance. We'll just derive a generic encode instance, and an encode instance too while we're at it. More information how this works is available in the argonaut docs:
```hs
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Generic.Rep (class Generic)

derive instance genericPhoneType :: Generic PhoneType _

instance encodeJsonPhoneType :: EncodeJson PhoneType where
  encodeJson = genericEncodeJson

instance decodeJsonPhoneType :: DecodeJson PhoneType where
  decodeJson = genericDecodeJson
```

Now we can save our `person` to local storage, but this isn't very useful unless we can retrieve the data. We'll tackle that next.

We'll start with retrieving the "person" string from local storage:
```hs
item <- getItem "person"
```

Then we'll create a helper function to handle converting the string from local storage to our `Person` record. Note that this string in storage may be `null`, so we represent it as a foreign `Json` until it is successfully decoded as a `String`. There are a number of other conversion steps along the way - each of which return an `Either` value, so it makes sense to organize these together in a `do` block.
```hs
processItem :: Json -> Either String Person
processItem item = do
  jsonString <- decodeJson item
  j <- jsonParser jsonString
  (p :: Person) <- decodeJson j
  pure p
```

Then we inspect this result to see if it succeeded. If it failed, we'll log the errors and use our default `examplePerson`, otherwise we'll use the person retrieved from local storage.
```hs
initialPerson <- case processItem item of
  Left err -> do
    log $ "Error: " <> err <> ". Loading examplePerson"
    pure examplePerson
  Right p -> pure p
```

Finally, we'll pass this `initialPerson` to our component via the `props` record:
```hs
-- Create JSX node from react component.
app = element addressBookApp { initialPerson }
```

And pick it up on the other side to use in our state hook:
```hs
mkAddressBookApp :: Effect (ReactComponent { initialPerson :: Person })
mkAddressBookApp =
  component "AddressBookApp" \props -> R.do
    Tuple person setPerson <- useState props.initialPerson
```

As a finishing touch, we'll improve the quality of our error messages by appending to the `String` of each `Left` value with `lmap`.
```hs
processItem :: Json -> Either String Person
processItem item = do
  jsonString <- lmap ("No string in local storage: " <> _) $ decodeJson item
  j <- lmap ("Cannot parse JSON string: " <> _) $ jsonParser jsonString
  (p :: Person) <- lmap ("Cannot decode Person: " <> _) $ decodeJson j
  pure p
```
Only the first error should ever occur during normal operation of this app. You can trigger the other errors by opening your web browser's dev tools, editing the saved "person" string in local storage, and refreshing the page. How you modify the JSON string determines which error is triggered. See if you can trigger each of them.

That covers local storage. Next we'll implement the `alert` action, which is very similar to the `log` action from the `Effect.Console` module. The only difference is that the `alert` action uses the `window.alert` method, whereas the `log` action uses the `console.log` method. As such, `alert` can only be used in environments where `window.alert` is defined, such as a web browser.

```hs
foreign import alert :: String -> Effect Unit
```

```js
exports.alert = msg => () =>
  window.alert(msg);
```

We want this alert to appear when either:
- A user attempts to save a form with validation errors.
- The state cannot be retrieved from local storage.

That is accomplished by simply replacing `log` with `alert` on these lines:
```hs
Left errs -> alert $ "There are " <> show (length errs) <> " validation errors."

alert $ "Error: " <> err <> ". Loading examplePerson"
```

 ## Exercises

 1. (Easy) Write a wrapper for the `removeItem` method on the `localStorage` object, and add your foreign function to the `Effect.Storage` module.
 1. (Medium) Add a "Reset" button that, when clicked, calls the newly-created `removeItem` function to delete the "person" entry from local storage.
 1. (Easy) Write a wrapper for the `confirm` method on the JavaScript `Window` object, and add your foreign function to the `Effect.Alert` module.
 1. (Medium) Call this `confirm` function when a users clicks the "Reset" button to ask if they're sure they want to reset their address book.

## Addendum

Todo -

In the spirit of being an "Introductory Text", I think the most important content to cover in this chapter is how to call JavaScript from PureScript, as is required for building the Address Book app. The details on runtime representation and calling PS from JS don't seem as necessary. I think it would be best to let these sections serve as an addendum, or even just live in the docs repo where they are partially duplicated already.

Some related discussion:
https://functionalprogramming.slack.com/archives/C04NA444H/p1588600540352500
https://functionalprogramming.slack.com/archives/C04NA444H/p1588600910356600
https://functionalprogramming.slack.com/archives/C04NA444H/p1588610399361200

Summary:
Phil originally had a JS app with a pure PS core when he wrote the book, but that use case is less common today, and creating a "reverse ffi" module for effectful purescript seems outside the scope of this book.

This guide has some details on embedding effectful PS within a React JS app.
https://thomashoneyman.com/articles/replace-react-components-with-purescript/#replacing-a-react-component-with-purescript-react


The content on


I don't think content on runtime representation and calling
Content on runtime representation

The below sections on runtime

runtime representations too.
Move these to end - What should this section be called:
Additional Content, Addendum?




## Calling PureScript from JavaScript

Calling a PureScript function from JavaScript is very simple, at least for functions with simple types.

Let's take the following simple module as an example:

```haskell
module Test where

gcd :: Int -> Int -> Int
gcd 0 m = m
gcd n 0 = n
gcd n m
  | n > m     = gcd (n - m) m
  | otherwise = gcd (m - n) n
```

This function finds the greatest common divisor of two numbers by repeated subtraction. It is a nice example of a case where you might like to use PureScript to define the function, but have a requirement to call it from JavaScript: it is simple to define this function in PureScript using pattern matching and recursion, and the implementor can benefit from the use of the type checker.

To understand how this function can be called from JavaScript, it is important to realize that PureScript functions always get turned into JavaScript functions of a single argument, so we need to apply its arguments one-by-one:

```javascript
var Test = require('Test');
Test.gcd(15)(20);
```

Here, I am assuming that the code was compiled with `spago build`, which compiles PureScript modules to CommonJS modules. For that reason, I was able to reference the `gcd` function on the `Test` object, after importing the `Test` module using `require`.

You might also like to bundle JavaScript code for the browser, using `spago bundle-app --to file.js`. In that case, you would access the `Test` module from the global PureScript namespace, which defaults to `PS`:

```javascript
var Test = PS.Test;
Test.gcd(15)(20);
```

## Understanding Name Generation

PureScript aims to preserve names during code generation as much as possible. In particular, most identifiers which are neither PureScript nor JavaScript keywords can be expected to be preserved, at least for names of top-level declarations.

If you decide to use a JavaScript keyword as an identifier, the name will be escaped with a double dollar symbol. For example,

```haskell
null = []
```

generates the following JavaScript:

```javascript
var $$null = [];
```

In addition, if you would like to use special characters in your identifier names, they will be escaped using a single dollar symbol. For example,

```haskell
example' = 100
```

generates the following JavaScript:

```javascript
var example$prime = 100;
```

Where compiled PureScript code is intended to be called from JavaScript, it is recommended that identifiers only use alphanumeric characters, and avoid JavaScript keywords. If user-defined operators are provided for use in PureScript code, it is good practice to provide an alternative function with an alphanumeric name for use in JavaScript.

## Runtime Data Representation

Types allow us to reason at compile-time that our programs are "correct" in some sense - that is, they will not break at runtime. But what does that mean? In PureScript, it means that the type of an expression should be compatible with its representation at runtime.

For that reason, it is important to understand the representation of data at runtime to be able to use PureScript and JavaScript code together effectively. This means that for any given PureScript expression, we should be able to understand the behavior of the value it will evaluate to at runtime.

The good news is that PureScript expressions have particularly simple representations at runtime. It should always be possible to understand the runtime data representation of an expression by considering its type.

For simple types, the correspondence is almost trivial. For example, if an expression has the type `Boolean`, then its value `v` at runtime should satisfy `typeof v === 'boolean'`. That is, expressions of type `Boolean` evaluate to one of the (JavaScript) values `true` or `false`. In particular, there is no PureScript expression of type `Boolean` which evaluates to `null` or `undefined`.

A similar law holds for expressions of type `Int` `Number` and `String` - expressions of type `Int` or `Number` evaluate to non-null JavaScript numbers, and expressions of type `String` evaluate to non-null JavaScript strings. Expressions of type `Int` will evaluate to integers at runtime, even though they cannot not be distinguished from values of type `Number` by using `typeof`.

What about some more complex types?

As we have already seen, PureScript functions correspond to JavaScript functions of a single argument. More precisely, if an expression `f` has type `a -> b` for some types `a` and `b`, and an expression `x` evaluates to a value with the correct runtime representation for type `a`, then `f` evaluates to a JavaScript function, which when applied to the result of evaluating `x`, has the correct runtime representation for type `b`. As a simple example, an expression of type `String -> String` evaluates to a function which takes non-null JavaScript strings to non-null JavaScript strings.

As you might expect, PureScript's arrays correspond to JavaScript arrays. But remember - PureScript arrays are homogeneous, so every element has the same type. Concretely, if a PureScript expression `e` has type `Array a` for some type `a`, then `e` evaluates to a (non-null) JavaScript array, all of whose elements have the correct runtime representation for type `a`.

We've already seen that PureScript's records evaluate to JavaScript objects. Just as for functions and arrays, we can reason about the runtime representation of data in a record's fields by considering the types associated with its labels. Of course, the fields of a record are not required to be of the same type.

## Representing ADTs

For every constructor of an algebraic data type, the PureScript compiler creates a new JavaScript object type by defining a function. Its constructors correspond to functions which create new JavaScript objects based on those prototypes.

For example, consider the following simple ADT:

```haskell
data ZeroOrOne a = Zero | One a
```

The PureScript compiler generates the following code:

```javascript
function One(value0) {
    this.value0 = value0;
};

One.create = function (value0) {
    return new One(value0);
};

function Zero() {
};

Zero.value = new Zero();
```

Here, we see two JavaScript object types: `Zero` and `One`. It is possible to create values of each type by using JavaScript's `new` keyword. For constructors with arguments, the compiler stores the associated data in fields called `value0`, `value1`, etc.

The PureScript compiler also generates helper functions. For constructors with no arguments, the compiler generates a `value` property, which can be reused instead of using the `new` operator repeatedly. For constructors with one or more arguments, the compiler generates a `create` function, which takes arguments with the appropriate representation and applies the appropriate constructor.

What about constructors with more than one argument? In that case, the PureScript compiler also creates a new object type, and a helper function. This time, however, the helper function is curried function of two arguments. For example, this algebraic data type:

```haskell
data Two a b = Two a b
```

generates this JavaScript code:

```javascript
function Two(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
};

Two.create = function (value0) {
    return function (value1) {
        return new Two(value0, value1);
    };
};
```

Here, values of the object type `Two` can be created using the `new` keyword, or by using the `Two.create` function.

The case of newtypes is slightly different. Recall that a newtype is like an algebraic data type, restricted to having a single constructor taking a single argument. In this case, the runtime representation of the newtype is actually the same as the type of its argument.

For example, this newtype representing telephone numbers:

```haskell
newtype PhoneNumber = PhoneNumber String
```

is actually represented as a JavaScript string at runtime. This is useful for designing libraries, since newtypes provide an additional layer of type safety, but without the runtime overhead of another function call.

## Representing Quantified Types

Expressions with quantified (polymorphic) types have restrictive representations at runtime. In practice, this means that there are relatively few expressions with a given quantified type, but that we can reason about them quite effectively.

Consider this polymorphic type, for example:

```haskell
forall a. a -> a
```

What sort of functions have this type? Well, there is certainly one function with this type - namely, the `identity` function, defined in the `Prelude`:

```haskell
id :: forall a. a -> a
id a = a
```

In fact, the `identity` function is the _only_ (total) function with this type! This certainly seems to be the case (try writing an expression with this type which is not observably equivalent to `identity`), but how can we be sure? We can be sure by considering the runtime representation of the type.

What is the runtime representation of a quantified type `forall a. t`? Well, any expression with the runtime representation for this type must have the correct runtime representation for the type `t` for any choice of type `a`. In our example above, a function of type `forall a. a -> a` must have the correct runtime representation for the types `String -> String`, `Number -> Number`, `Array Boolean -> Array Boolean`, and so on. It must take strings to strings, numbers to numbers, etc.

But that is not enough - the runtime representation of a quantified type is more strict than this. We require any expression to be _parametrically polymorphic_ - that is, it cannot use any information about the type of its argument in its implementation. This additional condition prevents problematic implementations such as the following JavaScript function from inhabiting a polymorphic type:

```javascript
function invalid(a) {
    if (typeof a === 'string') {
        return "Argument was a string.";
    } else {
        return a;
    }
}
```

Certainly, this function takes strings to strings, numbers to numbers, etc. but it does not meet the additional condition, since it inspects the (runtime) type of its argument, so this function would not be a valid inhabitant of the type `forall a. a -> a`.

Without being able to inspect the runtime type of our function argument, our only option is to return the argument unchanged, and so `identity` is indeed the only inhabitant of the type `forall a. a -> a`.

A full discussion of _parametric polymorphism_ and _parametricity_ is beyond the scope of this book. Note however, that since PureScript's types are _erased_ at runtime, a polymorphic function in PureScript _cannot_ inspect the runtime representation of its arguments (without using the FFI), and so this representation of polymorphic data is appropriate.

## Representing Constrained Types

Todo - Remove this section as described in https://github.com/purescript/documentation/issues/273

Functions with a type class constraint have an interesting representation at runtime. Because the behavior of the function might depend on the type class instance chosen by the compiler, the function is given an additional argument, called a _type class dictionary_, which contains the implementation of the type class functions provided by the chosen instance.

For example, here is a simple PureScript function with a constrained type which uses the `Show` type class:

```haskell
shout :: forall a. Show a => a -> String
shout a = show a <> "!!!"
```

The generated JavaScript looks like this:

```javascript
var shout = function (dict) {
    return function (a) {
        return show(dict)(a) + "!!!";
    };
};
```

Notice that `shout` is compiled to a (curried) function of two arguments, not one. The first argument `dict` is the type class dictionary for the `Show` constraint. `dict` contains the implementation of the `show` function for the type `a`.

We can call this function from JavaScript by passing an explicit type class dictionary from `Data.Show` as the first parameter:

```javascript
shout(require('Data.Show').showNumber)(42);
```

 ## Exercises

 1. (Easy) What are the runtime representations of these types?

     ```haskell
     forall a. a
     forall a. a -> a -> a
     forall a. Ord a => Array a -> Boolean
     ```

     What can you say about the expressions which have these types?
 1. (Medium) Try using the functions defined in the `arrays` package, calling them from JavaScript, by compiling the library using `spago build` and importing modules using the `require` function in NodeJS. _Hint_: you may need to configure the output path so that the generated CommonJS modules are available on the NodeJS module path.


## Representing Side Effects

The `Effect` monad is also defined as a foreign type. Its runtime representation is quite simple - an expression of type `Effect a` should evaluate to a JavaScript function of **no arguments**, which performs any side-effects and returns a value with the correct runtime representation for type `a`.

The definition of the `Effect` type constructor is given in the `Effect` module as follows:

```haskell
foreign import data Effect :: Type -> Type
```

As a simple example, consider the `random` function defined in the `random` package. Recall that its type was:

```haskell
foreign import random :: Effect Number
```

The definition of the `random` function is given here:

```javascript
exports.random = Math.random;
```

Notice that the `random` function is represented at runtime as a function of no arguments. It performs the side effect of generating a random number, and returns it, and the return value matches the runtime representation of the `Number` type: it is a non-null JavaScript number.

As a slightly more interesting example, consider the `log` function defined by the `Effect.Console` module in the `console` package. The `log` function has the following type:

```haskell
foreign import log :: String -> Effect Unit
```

And here is its definition:

```javascript
exports.log = function (s) {
  return function () {
    console.log(s);
  };
};
```

The representation of `log` at runtime is a JavaScript function of a single argument, returning a function of no arguments. The inner function performs the side-effect of writing a message to the console.

Expressions of type `Effect a` can be invoked from JavaScript like regular JavaScript methods. For example, since the `main` function is required to have type `Effect a` for some type `a`, it can be invoked as follows:

```javascript
require('Main').main();
```

When using `spago bundle-app --to` or `spago run`, this call to `main` is generated automatically, whenever the `Main` module is defined.


## Below sections replaced by argonaut

## Working With Untyped Data

In this section, we will see how we can use the `Foreign` library to turn untyped data into typed data, with the correct runtime representation for its type.

The code for this chapter demonstrates how a record can be serialized to JSON and then stored in and retrieved from local storage.

The `Main` module defines a type for the saved form data:

```haskell
newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  }
```

The problem is that we have no guarantee that the JSON will have the correct form. Put another way, we don't know that the JSON represents the correct type of data at runtime. This is the sort of problem that is solved by the `foreign` library. Here are some other examples:

- A JSON response from a web service
- A value passed to a function from JavaScript code

Let's try the `foreign` and `foreign-generic` libraries in PSCi.

Start by importing some modules:

```text
> import Foreign
> import Foreign.Generic
> import Foreign.JSON
```

A good way to obtain a `Foreign` value is to parse a JSON document. `foreign-generic` defines the following two functions:

```haskell
parseJSON :: String -> F Foreign
decodeJSON :: forall a. Decode a => String -> F a
```

The type constructor `F` is actually just a type synonym, defined in `Foreign`:

```haskell
type F = Except MultipleErrors
```

Here, `Except` is a monad for handling exceptions in pure code, much like `Either`. We can convert a value in the `F` monad into a value in the `Either` monad by using the `runExcept` function.

Most of the functions in the `foreign` and `foreign-generic` libraries return a value in the `F` monad, which means that we can use do notation and the applicative functor combinators to build typed values.

The `Decode` type class represents those types which can be obtained from untyped data. There are type class instances defined for the primitive types and arrays, and we can define our own instances as well.

Let's try parsing some simple JSON documents using `decodeJSON` in PSCi (remembering to use `runExcept` to unwrap the results):

```text
> import Control.Monad.Except

> runExcept (decodeJSON "\"Testing\"" :: F String)
Right "Testing"

> runExcept (decodeJSON "true" :: F Boolean)
Right true

> runExcept (decodeJSON "[1, 2, 3]" :: F (Array Int))
Right [1, 2, 3]
```

Recall that in the `Either` monad, the `Right` data constructor indicates success. Note however, that invalid JSON, or an incorrect type leads to an error:

```text
> runExcept (decodeJSON "[1, 2, true]" :: F (Array Int))
(Left (NonEmptyList (NonEmpty (ErrorAtIndex 2 (TypeMismatch "Int" "Boolean")) Nil)))
```

The `foreign-generic` library tells us where in the JSON document the type error occurred.

## Handling Null and Undefined Values

Real-world JSON documents contain null and undefined values, so we need to be able to handle those too. `foreign-generic` solves this problem with the 'Maybe' type constructor to represent missing values.

Todo - the content about NullOrUndefined versus Maybe seems good to include from original

```text
> import Prelude
> import Foreign.NullOrUndefined

> runExcept (decodeJSON "42" :: F (Maybe Int))
(Right (Just 42))

> runExcept (decodeJSON "null" :: F (Maybe Int))
(Right Nothing)
```

The type `Maybe Int` represents values which are either integers, or null. What if we wanted to parse more interesting values, like arrays of integers, where each element might be `null`? `decodeJSON` handles such cases as well:

```text
> runExcept (decodeJSON "[1,2,null]" :: F (Array (Maybe Int)))
(Right [(Just 1),(Just 2),Nothing])
```

## Generic JSON Serialization

In fact, we rarely need to write instances for the `Decode` class, since the `foreign-generic` class allows us to _derive_ instances using a technique called _datatype-generic programming_. A full explanation of this technique is beyond the scope of this book, but it allows us to write functions once, and reuse them over many different data types, based on the structure of a the types themselves.

To derive a `Decode` instance for our `FormData` type (so that we may deserialize it from its JSON representation), we first use the `derive` keyword to derive an instance of the `Generic` type class, which looks like this:

```haskell
import Data.Generic.Rep
derive instance genericFormData :: Generic FormData _
```

Next, we simply define the `decode` function using the `genericDecode` function, as follows:

```haskell
import Foreign.Class
instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
```

In fact, we can also derive an _encoder_ in the same way:

```haskell
instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
```
Todo - probably don't need generic show for this if it's a record.

And even an instance of Show which comes in handy for logging the result:

```haskell
instance showFormData :: Show FormData where
  show = genericShow
```

It is important that we use the same options in the decoder and encoder, otherwise our encoded JSON documents might not get decoded correctly.

Now, in our main function, a value of type `FormData` is passed to the `encode` function, serializing it as a JSON document. The `FormData` type is a newtype for a record, so a value of type `FormData` passed to `encode` will be serialized as a JSON _object_. This is because we used the `unwrapSingleConstructors` option when defining our JSON encoder.

Our `Decode` type class instance is used with `decodeJSON` to parse the JSON document when it is retrieved from local storage, as follows:

```haskell
loadSavedData = do
  item <- getItem "person"

  let
    savedData :: Either (NonEmptyList ForeignError) (Maybe FormData)
    savedData = runExcept do
      jsonOrNull <- traverse readString =<< readNullOrUndefined item
      traverse decodeJSON jsonOrNull
```

The `savedData` action reads the `FormData` structure in two steps: first, it parses the `Foreign` value obtained from `getItem`. The type of `jsonOrNull` is inferred by the compiler to be `Maybe String` (exercise for the reader - how is this type inferred?). The `traverse` function is then used to apply `decodeJSON` to the (possibly missing) element of the result of type `Maybe String`. The type class instance inferred for `decodeJSON` is the one we just wrote, resulting in a value of type `F (Maybe FormData)`.

We need to use the monadic structure of `F`, since the argument to `traverse` uses the result `jsonOrNull` obtained in the first line.

There are three possibilities for the result of `FormData`:

- If the outer constructor is `Left`, then there was an error parsing the JSON string, or it represented a value of the wrong type. In this case, the application displays an error using the `alert` action we wrote earlier.
- If the outer constructor is `Right`, but the inner constructor is `Nothing`, then `getItem` also returned `Nothing` which means that the key did not exist in local storage. In this case, the application continues quietly.
- Finally, a value matching the pattern `Right (Just _)` indicates a successfully parsed JSON document. In this case, the application updates the form fields with the appropriate values.

Try out the code, by running `spago bundle-app --to dist/Main.js`, and then opening the browser to `html/index.html`. You should be able to see what's going on in the console.

_Note_: You may need to serve the HTML and JavaScript files from a HTTP server locally in order to avoid certain browser-specific issues.
