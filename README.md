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
  `AsValidation`) following the conventions of `makeClassy` and
  `makeClassyPrisms` from `lens`.
* Polymorphic prisms (`__Failure`, `__Success`) for type-changing operations.
* Isomorphisms to `Either` and `(Bool, a)`.

## `ValidationMonadT`

`ValidationMonadT err m a` is a monad transformer wrapping `m (Validation err a)`.
Unlike `Validation`, it has short-circuiting `Applicative`, `Bind`, `Monad`,
and `MonadError` instances.

`ValidationMonad err a` is a type alias for `ValidationMonadT err Identity a`.

## Validators

The library provides four validator newtypes, each wrapping a validation
function with a different type parameter order to enable different class
instances:

| Type | Wraps | Key instances |
|------|-------|---------------|
| `Validator x err a` | `x -> Validation err a` | `Bifunctor`, accumulating `Applicative` |
| `ValidatorProfunctor err x a` | `x -> Validation err a` | `Profunctor`, accumulating `Applicative` |
| `ValidatorMonadT x err f a` | `x -> ValidationMonadT err f a` | `Monad`, `MonadTrans`, `BindTrans` |
| `ValidatorMonadProfunctorT err f x a` | `x -> ValidationMonadT err f a` | `Profunctor`, `Monad`, `Category`, `Arrow` |

All four are isomorphic and have cross-type optics instances for converting
between them.
