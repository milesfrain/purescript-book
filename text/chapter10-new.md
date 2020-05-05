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

`src/Data/Calculate.js`:
```js
"use strict";

exports.square = function(n) {
  return n * n;
};
```

`src/Data/Calculate.purs`:
```hs
module Data.Calculate where

foreign import square :: Number -> Number
```

```text
$ spago repl

> import Data.Calculate
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

> import Data.Calculate
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
exports.diagonalArrow = w => h => Math.sqrt(w * w + h * h);
```

```hs
foreign import diagonalNested :: Number -> Number -> Number
foreign import diagonalArrow  :: Number -> Number -> Number
```

```text
$ spago repl

> import Data.Calculate
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

```haskell
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
var curriedAdd = n => m => m + n | 0;
```

The remaining examples in this book will use arrow functions instead of nested functions.

## Exercises
1. (Medium) Write a JavaScript function `volumeFn` in the `Data.Calculate` module that finds the volume of a box. Use an `Fn` wrapper from `Data.Function.Uncurried`.
2. (Medium) Rewrite `volumeFn` with arrow functions as `volumeArrow`.

## Beyond Simple Types

Suppose we wanted to recreate the `head` function on arrays by using a foreign declaration. In JavaScript, we might write the function as follows:

```javascript
exports.head = function(arr) {
  return arr[0];
}
```

However, there is a problem with this function. We might try to give it the type `forall a. Array a -> a`, but for empty arrays, this function returns `undefined`. Therefore, this function does not have the correct runtime representation.

We can instead return a `Maybe` value to handle this corner case.

It is tempting to write the following:
```js
exports.maybeHead = function(arr) {
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
exports.maybeHeadImpl = function(just, nothing, arr) {
  if (arr.length) {
    return just(arr[0]);
  } else {
    return nothing;
  }
}
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

## Defining Foreign Types

Suppose instead of returning a `Maybe a`, we wanted to return a new type `Undefined a` whose representation at runtime was like that for the type `a`, but also allowing the `undefined` value.

head
maybeHead
undefinedHead
unsafeHead

Recall that PureScript shares three primitive types with JavaScript: `Number`, `String`, and `Boolean`.

## Foreign Types

## Effectful Functions


