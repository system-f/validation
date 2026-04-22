# Validation

![System F Logo](https://logo.systemf.com.au/systemf-450x450.png)

A data type like `Either` but with an accumulating `Applicative` instance.

Download from [hackage](http://hackage.haskell.org/package/validation).

## `Validation`

The `Validation` data type is isomorphic to `Either`, but has an instance
of `Applicative` that accumulates on the error side. That is to say, if two
(or more) errors are encountered, they are appended using a `Semigroup`
operation.

As a consequence of this `Applicative` instance, there is no corresponding
`Bind` or `Monad` instance. `Validation` is an example of, "An applicative
functor that is not a monad."

The library provides:

* Classy optics (`GetValidation`, `HasValidation`, `ReviewValidation`,
  `AsValidation`, and corresponding classes for `Failure` and `Success`)
  following the conventions of `makeClassy` and `makeClassyPrisms` from `lens`.
* Polymorphic prisms (`__Failure`, `__Success`) for type-changing operations.
* Isomorphisms to `Either` and `(Bool, a)`.

## `Validator`

The `Validator` newtype is a profunctor transformer:

```haskell
newtype Validator e p x a = Validator (p x (Validation e a))
```

`Validator e (->) x a` is isomorphic to `x -> Validation e a`. The profunctor
parameter `p` generalises this to other optic-like contexts such as `Tagged`,
`Iso`, and `Prism`.

Instances include `Functor`, `Apply`, `Applicative`, `Alt`, `Selective`,
`Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category`, `Arrow`,
`ArrowApply`, `ArrowChoice`, and `Wrapped`.

The `Applicative` instance accumulates errors in parallel (using `Semigroup`),
while `Category` composition short-circuits on `Failure` (like monadic bind).

The library also provides profunctor newtype wrappers (`Iso''`, `Prism''`) that
allow `Validator` to be parameterised over monomorphic isos and prisms.
