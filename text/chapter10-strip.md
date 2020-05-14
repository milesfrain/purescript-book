
Todo - this was part of the initial calling JS section as a transition to wrapping.

This approach works well for simple JavaScript values, but is of limited use for more complicated examples. The reason is that most idiomatic JavaScript code does not meet the strict criteria imposed by the runtime representations of the basic PureScript types. In those cases, we have another option - we can _wrap_ the JavaScript code in such a way that we can force it to adhere to the correct runtime representation.

## Wrapping JavaScript Values

We might want to wrap JavaScript values and functions for a number of reasons:

- A function takes multiple arguments, but we want to call it like a curried function. Todo - how does this work with previous section?
- We might want to use the `Effect` monad to keep track of any JavaScript side-effects.
- It might be necessary to handle corner cases like `null` or `undefined`, to give a function the correct runtime representation.



