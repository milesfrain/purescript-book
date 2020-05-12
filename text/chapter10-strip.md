
Todo - this was part of the initial calling JS section as a transition to wrapping.

This approach works well for simple JavaScript values, but is of limited use for more complicated examples. The reason is that most idiomatic JavaScript code does not meet the strict criteria imposed by the runtime representations of the basic PureScript types. In those cases, we have another option - we can _wrap_ the JavaScript code in such a way that we can force it to adhere to the correct runtime representation.

## Wrapping JavaScript Values

We might want to wrap JavaScript values and functions for a number of reasons:

- A function takes multiple arguments, but we want to call it like a curried function. Todo - how does this work with previous section?
- We might want to use the `Effect` monad to keep track of any JavaScript side-effects.
- It might be necessary to handle corner cases like `null` or `undefined`, to give a function the correct runtime representation.




## Conclusion

In this chapter, we've learned how to work with foreign JavaScript code from PureScript, and vice versa, and we've seen the issues involved with writing trustworthy code using the FFI:

- We've seen the importance of the _runtime representation_ of data, and ensuring that foreign functions have the correct representation.
- We learned how to deal with corner cases like null values and other types of JavaScript data, by using foreign types, or the `Foreign` data type.
- We looked at some common foreign types defined in the Prelude, and how they can be used to interoperate with idiomatic JavaScript code. In particular, the representation of side-effects in the `Effect` monad was introduced, and we saw how to use the `Effect` monad to capture new side effects.
- We saw how to safely deserialize JSON data using the `Decode` type class.

For more examples, the `purescript`, `purescript-contrib` and `purescript-node` GitHub organizations provide plenty of examples of libraries which use the FFI. In the remaining chapters, we will see some of these libraries put to use to solve real-world problems in a type-safe way.
