# CLAUDE.md — Haskell Project Instructions

## Company Name Spelling

When referencing the company in copyright notices, documentation, or any other text, always spell it exactly as **Sirius-beta Labs** (capital "S", lowercase "beta" after the hyphen, capital "L"). Do not write it as "Sirius Beta Labs", "SiriusBeta Labs", "Sirius-Beta Labs", or any other variant.

## Core Principle: Type Class-Driven Development

Design data types to admit maximal lawful type class instances. Implement functionality through type class methods and optics rather than ad-hoc top-level functions.

## Preferred Package Ecosystem

Design data types that can integrate with instances from these packages:

* **adjunctions** — Representable functors and adjunctions
* **alignment** — Semialign, Align, Unalign
* **associative** — Associative operations
* **base** — Foundation (Functor, Applicative, Monad, Alternative, etc.)
* **bifunctors** — Bifunctor hierarchy
* **comonad** — Comonad hierarchy (https://hackage-content.haskell.org/package/comonad)
* **containers** — Standard data structures (Map, Set, Seq, etc.)
* **contravariant** — Contravariant, Divisible, Decidable
* **deepseq** — NFData and deep evaluation
* **distributive** — Distributive functors
* **free** — Free monads and cofree comonads
* **id** — Identity functors and related abstractions
* **lens** — Optics and indexed operations
* **mtl** — MonadReader, MonadWriter, MonadState, MonadError, MonadCont, etc.
* **one** — Singleton types
* **polytree** — Tree structures
* **product-profunctors** — Profunctors with product and sum structure (https://hackage.haskell.org/package/product-profunctors)
* **profunctors** — Profunctor hierarchy
* **selective** — Selective functors
* **semigroupoids** — Apply, Bind, Alt, Plus, etc.
* **syb** — Data and generic traversals
* **transformers** — Monad transformers, MonadTrans, MonadIO
* **witherable** — Filterable and Witherable

## Mandatory Type Class Instances

**CRITICAL**: Only implement instances that are *lawful* and *canonical*.

- **Lawful**: The instance satisfies all laws defined by the type class
- **Canonical**: There is an obvious, principled implementation; not arbitrary

**IMPORTANT**: When implementing `Functor`, `Foldable`, `Traversable`, `Bifunctor`, or `Filterable` instances, immediately add the corresponding fusion rules using `{-# RULES #-}` pragmas. Write the rules on top-level worker functions, never on the class methods themselves (see [Fusion Rules](#fusion-rules-apply-aggressively)). Fusion rules eliminate intermediate data structures and are mandatory for these instances.

For every data type, implement all instances from this list where a lawful canonical instance exists:

### Structural and Generic
* `Generic` — Enable generic deriving mechanisms
* `Generic1` — Generic for higher-kinded types
* `Data` — Generic traversals and queries (from syb)
* `Typeable` — Runtime type information
* `NFData` — Deep strictness evaluation (from deepseq)

### Equality and Ordering
* `Eq`, `Eq1`, `Eq2` — Equality comparison
* `Ord`, `Ord1`, `Ord2` — Total ordering
* `Show`, `Show1`, `Show2` — Human-readable string representation

### Algebraic Structures
* `Semigroup` — Associative binary operation
* `Monoid` — Semigroup with identity element

### Functor Hierarchy
* `Functor` — Covariant mapping
* `Apply` — Applicative without `pure` (semigroupoids)
* `Applicative` — Applicative functor with `pure`
* `Bind` — Monad without `return` (semigroupoids)
* `Monad` — Monadic sequencing
* `MonadFail` — Explicit pattern match failures
* `MonadFix` — Recursive monadic bindings
* `MonadZip` — Zip two monadic structures

### Alternative and Choice
* `Alt` — Associative operation without identity (semigroupoids)
* `Plus` — Alt with identity (semigroupoids)
* `Alternative` — Applicative with choice
* `MonadPlus` — Monadic choice with failure

### Foldable and Traversable
* `Foldable` — Fold structure to summary value
* `Foldable1` — Fold non-empty structure (semigroupoids)
* `Traversable` — Traverse with effects, preserving structure
* `Traversable1` — Traverse non-empty structure (semigroupoids)

### Indexed Operations
* `FunctorWithIndex` — Functor with access to position indices (lens)
* `FoldableWithIndex` — Foldable with access to indices (lens)
* `TraversableWithIndex` — Traversable with access to indices (lens)

### Lens Type Classes for Data Structures
* `At` — Index containers with arbitrary insertion/deletion (lens)
* `Ixed` — Indexed access to elements (lens)
* `Contains` — Test membership in container (lens)
* `Cons` — Prepend element to sequence (lens)
* `Snoc` — Append element to sequence (lens)
* `Each` — Traverse every element of a homogeneous container (lens)
* `Empty` — Empty value and emptiness testing (lens)
* `Reversing` — Reverse a sequence (lens)

### Lens Type Classes for Plated Recursion
* `Plated` — Uniplate-style recursive traversal (lens)
* `GPlated` — Generically derive Plated instances (lens)
* `GPlated1` — Generically derive Plated for higher-kinded types (lens)

### Lens Type Classes for Text Operations
* `Prefixed` — Strip/match prefix of text (lens)
* `Suffixed` — Strip/match suffix of text (lens)

### Lens Type Classes for Traversal Optimization
* `TraverseMin` — Traverse minimum element (lens)
* `TraverseMax` — Traverse maximum element (lens)

### Lens Type Classes for Tuples
* `Field1` — Access first field of tuple (lens)
* `Field2` — Access second field of tuple (lens)
* `Field3` — Access third field of tuple (lens)
* `Field4` — Access fourth field of tuple (lens)
* `Field5` — Access fifth field of tuple (lens)
* `Field6` — Access sixth field of tuple (lens)
* `Field7` — Access seventh field of tuple (lens)
* `Field8` — Access eighth field of tuple (lens)
* `Field9` — Access ninth field of tuple (lens)

### Lens Internal Type Classes
* `Settable` — Functors that support setting (lens, internal use)
* `Reviewable` — Functors that support reviewing (lens, internal use)

### Comonads
* `Extend` — Comonadic extension without extract (semigroupoids)
* `Comonad` — Comonadic extraction and duplication (comonad)
* `ComonadApply` — Comonadic application (comonad)
* `ComonadTraced` — Comonad with traced environment (comonad)
* `ComonadStore` — Comonad with stored position (comonad)
* `ComonadEnv` — Comonad with environment (comonad)
* `ComonadHoist` — Natural transformation on comonad layers (comonad)
* `ComonadTrans` — Comonad transformer lifting (comonad)

### Bifunctors
* `Bifunctor` — Map over two type parameters independently (bifunctors)
* `Biapply` — Apply for bifunctors (semigroupoids)
* `Biapplicative` — Bifunctor with `bipure` (bifunctors)
* `Bifoldable` — Fold both type parameters (bifunctors)
* `Bifoldable1` — Fold both parameters, non-empty (semigroupoids)
* `Bitraversable` — Traverse both parameters with effects (bifunctors)
* `Bitraversable1` — Traverse both parameters, non-empty (semigroupoids)
* `Swap` — Swap the two type parameters (bifunctors)

### Profunctors
* `Profunctor` — Contravariant in first param, covariant in second (profunctors)
* `Strong` — Profunctor preserving products (profunctors)
* `Choice` — Profunctor preserving sums (profunctors)
* `Closed` — Profunctor preserving exponentials (profunctors)
* `Costrong` — Dual to Strong (profunctors)
* `Cochoice` — Dual to Choice (profunctors)
* `Mapping` — Profunctor preserving Functor structure (profunctors)
* `Traversing` — Profunctor preserving Traversable structure (profunctors)
* `Sieve` — Profunctor representable by reader (profunctors)
* `Cosieve` — Dual to Sieve (profunctors)
* `Corepresentable` — Profunctor representable by writer (profunctors)
* `ProfunctorAdjunction` — Adjunction between profunctors (profunctors)
* `ProfunctorFunctor` — Functor on profunctors (profunctors)
* `ProfunctorMonad` — Monad on profunctors (profunctors)
* `ProfunctorComonad` — Comonad on profunctors (profunctors)
* `ProductProfunctor` — Profunctor with product structure (product-profunctors)
* `SumProfunctor` — Profunctor with sum structure (product-profunctors)

### Categories and Arrows
* `Semigroupoid` — Associative composition without identity (semigroupoids)
* `Category` — Composition with identity
* `Arrow` — Generalized function arrows
* `ArrowZero` — Arrow with failure
* `ArrowPlus` — Arrow with choice
* `ArrowChoice` — Arrow operating on sum types
* `ArrowApply` — Arrow with application
* `ArrowLoop` — Arrow with recursion

### Contravariant Functors
* `Contravariant` — Contravariant mapping (contravariant)
* `Divisible` — Contravariant applicative (contravariant)
* `Decidable` — Contravariant alternative (contravariant)

### Monad Transformers
* `MonadTrans` — Lift operations through transformer stack (transformers)
* `BindTrans` — Bind through transformer (semigroupoids)
* `MonadReader` — Read-only environment (mtl)
* `MonadWriter` — Accumulated output (mtl)
* `MonadState` — Mutable state (mtl)
* `MonadRWS` — Combined reader/writer/state (mtl)
* `MonadError` — Exceptions with recovery (mtl)
* `MonadIO` — Lift IO operations (transformers)
* `MonadCont` — Continuation-passing style (mtl)

### Specialized Functors
* `Distributive` — Distribute functors (distributive)
* `Representable` — Isomorphic to function from fixed type (adjunctions)
* `Selective` — Selective applicative functors (selective)
* `Filterable` — Filter and map simultaneously (witherable)
* `Witherable` — Filterable with effects (witherable)

## Documentation and Project Structure

### Provide Extensive Type Aliases

For data types with multiple type parameters, provide convenient type aliases for common instantiations:

```haskell
-- Main data type
data Validated f e a = Validated (f a) [e]

-- Common instantiation: specific error container
type Validated' e a = Validated Maybe e a

-- Specific instantiations for common functors
type ValidatedList e a = Validated [] e a
type ValidatedMaybe e a = Validated Maybe e a
type ValidatedEither e a = Validated (Either e) e a
type ValidatedIO e a = Validated IO e a

-- For wrapper types, provide many aliases
type WrapMaybe a = Wrap Maybe a
type WrapIdentity a = Wrap Identity a
type WrapList a = Wrap [] a
type WrapNonEmpty a = Wrap NonEmpty a
type WrapEither e a = Wrap (Either e) a
```

**Benefits:**
- Reduces type signature noise
- Makes common cases immediately obvious
- Improves error messages
- Easier to read and write code

### Write Comprehensive FUSION.md

For performance-critical libraries, document fusion rules in detail:

```markdown
# Fusion Rules in [package-name]

## What is Fusion?

[Explanation of fusion for this package]

## Rule Categories

### 1. Functor Fusion
- `mapX/mapX` rule (on the worker, not on `fmap`)
- What it does
- Example optimization
- Based on which law

### 2. [Other categories]

## Phase Control

[Explain phase annotations]

## Verifying Fusion

```bash
cabal build --ghc-options="-ddump-rule-firings"
```

## Performance Impact

[Benchmarks and use cases]

## Law Foundation

[Table mapping rules to laws]
```

See performance-critical libraries for complete examples.

### Use CPP for Compatibility (Sparingly)

```haskell
{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
```

Use CPP only for cross-version compatibility. Keep blocks minimal and test on all supported GHC versions.

### Document Mathematical Laws

```haskell
-- | A setoid (equivalence relation).
--
-- Laws: Reflexive, Symmetric, Transitive
--
-- >>> check $ property $ do x <- forAll gen; lawSetoidReflexive x === True
class Setoid a where
  equiv :: a -> a -> Bool

lawSetoidReflexive :: Setoid a => a -> Bool
lawSetoidReflexive x = x `equiv` x
```

Export law-checking functions for downstream verification.

### UndecidableInstances: Use When Necessary

**Acceptable uses:**
```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- Required: the context Eq (f (Fix f)) is no smaller than the instance
-- head Eq (Fix f), so it fails the Paterson conditions.
-- Terminates: for any concrete f (e.g. Maybe), resolving Eq (f (Fix f))
-- reaches Eq (Fix f) again, which is this same instance, so resolution
-- ties the knot rather than growing.

newtype Fix f = Fix (f (Fix f))

deriving newtype instance Eq (f (Fix f)) => Eq (Fix f)
deriving newtype instance Ord (f (Fix f)) => Ord (Fix f)
```

Without `UndecidableInstances`, GHC rejects this with "The constraint ‘Eq (f (Fix f))’ is no smaller than the instance head ‘Eq (Fix f)’". Do not enable the extension when GHC accepts the code without it. For example, `Show (f a) => Show (Wrap f a)` does not need it.

Use only when instance resolution terminates, and document why in a comment next to the pragma. This includes type-level computation, such as type families in instance contexts over a generic representation, provided the comment explains why the computation terminates (for example, that each recursive step is on a smaller component of the instance head). Never use it to enable overlapping instances.

## Never Use Record Selectors

**Never use record selectors (field accessor functions generated by record syntax).** They are partial when the type has multiple constructors and they bypass the optics-first approach.

**Instead, use optics** (lenses, prisms, traversals) to access and modify record fields:

```haskell
-- FORBIDDEN: Using record selectors
getName :: Person -> String
getName p = name p        -- Generated selector function

getAge :: Person -> Int
getAge = age              -- Point-free selector

-- CORRECT: Use optics
getName :: Person -> String
getName = view _name

getAge :: Person -> Int
getAge = view _age

-- CORRECT: Modify with optics
setName :: String -> Person -> Person
setName = set _name

updateAge :: Person -> Person
updateAge = _age %~ (+ 1)
```

**When defining data types with records**, always provide corresponding optics (via the GetX/HasX pattern or `makeLenses`) and access fields exclusively through those optics. The record syntax may still be used for construction and pattern matching where appropriate, but never use the generated selector functions.

## Forbidden Functions

### Never Use These Partial/Unsafe Functions

**NEVER use these functions.** Every single one has a safer, better alternative:

**All partial functions crash on invalid input.** Use optics, `Maybe`, `Either`, `NonEmpty`, or pattern matching instead.

#### Partial List Functions

```haskell
head        -- Use: preview _head, (^? ix 0), or pattern match (x:_)
tail        -- Use: preview _tail, or pattern match (_:xs)
(!!)        -- Use: (^? ix n), preview (ix n)
init        -- Use: preview _init or pattern match
last        -- Use: preview _last or NonEmpty
maximum     -- Use: maximumOf folded, maximum1 (Foldable1)
minimum     -- Use: minimumOf folded, minimum1 (Foldable1)
foldl1      -- Use: foldl1 from Data.Semigroup.Foldable (Foldable1)
foldr1      -- Use: foldr1 from Data.Semigroup.Foldable (Foldable1)
```

**Safe alternatives:**
```haskell
import Control.Lens (preview, _head, _last, ix, (^?), maximumOf, minimumOf, folded)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (maximum1, minimum1)

-- Optics approach
getHead xs = xs ^? _head
getAt n xs = xs ^? ix n
getLast xs = xs ^? _last
getMax xs = maximumOf folded xs
```

#### Partial Maybe/Error Functions

```haskell
fromJust    -- Use: fromMaybe, maybe, or pattern matching
error       -- Use: Either for typed errors
undefined   -- Use: typed holes (_) during development, remove before commit
```

#### Unsafe I/O and Coercion

**ABSOLUTELY FORBIDDEN:**
```haskell
unsafePerformIO, unsafeInterleaveIO, unsafeCoerce
unsafeIOToST, unsafeSTToIO, unsafeIOToSTM
```

These break type safety, referential transparency, and encapsulation. **NEVER use these.** Redesign using proper types and abstractions.

**Only exception:** Internal implementation of low-level libraries (`bytestring`, `text`, `vector`) where safety is proven and documented exhaustively.

#### Partial String Functions

```haskell
read        -- Use: readMaybe, readEither, or parser libraries (megaparsec, attoparsec)
```

**Summary:** Write total functions. Make failure explicit in types (`Maybe`, `Either`, `NonEmpty`). Never use `error`, `undefined`, or `unsafe*` in production.

## Forbidden Packages

### Never Depend on `relude`

**Do not depend on the [`relude`](https://hackage-content.haskell.org/package/relude) package.** Every function in `relude` has a better alternative — typically from `lens` or `semigroupoids` — that composes with the rest of the ecosystem this project targets.

If you encounter a `relude` function that cannot be expressed in terms of `lens`, `semigroupoids`, `witherable`, `base`, or the other packages listed under [Preferred Package Ecosystem](#preferred-package-ecosystem), **stop and ask the user** before introducing a new abstraction.

#### Replacements

Standard imports for the replacements below:

```haskell
import Control.Lens
import Control.Monad (join)
import Data.Functor.Apply (liftF2)
import Data.Maybe (fromMaybe)
import Witherable (wither)
```

**Duplication:**

```haskell
-- Duplicate a value into a pair
dup :: a -> (a, a)
dup = join (,)
```

**Tagging with a computed component:**

```haskell
-- Apply a function and put the result in the first slot
toFst :: (a -> b) -> a -> (b, a)
toFst f = over _1 f . dup

-- Apply a function and put the result in the second slot
toSnd :: (a -> b) -> a -> (a, b)
toSnd f = over _2 f . dup

-- Lifted through a Functor
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd = fmap . toSnd
```

**Effectful tagging:**

```haskell
-- Effectful variants using liftF2 from semigroupoids' Apply
traverseToFst :: Functor t => (a -> t b) -> a -> t (b, a)
traverseToFst = liftF2 fmap (flip (,))

traverseToSnd :: Functor t => (a -> t b) -> a -> t (a, b)
traverseToSnd = liftF2 fmap (,)

-- Traverse both components of a homogeneous pair
traverseBoth :: Applicative t => (a -> t b) -> (a, a) -> t (b, b)
traverseBoth = both
```

**`Either` projections and defaults:**

```haskell
-- Extract with a default, using prisms
fromLeft :: a -> Either a b -> a
fromLeft a = fromMaybe a . preview _Left

fromRight :: b -> Either a b -> b
fromRight b = fromMaybe b . preview _Right

-- Either <-> Maybe conversions
leftToMaybe :: Either l r -> Maybe l
leftToMaybe = preview _Left

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = preview _Right

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft = flip maybe Left . Right

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight = flip maybe Right . Left
```

**Effectful filter-map:**

```haskell
-- mapMaybe with effects, via witherable
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM = wither
```

Prefer these formulations over reaching for `relude`. They compose with the rest of the optics- and semigroupoids-based ecosystem this project standardises on.

## Avoid Boolean Blindness

**Prefer descriptive sum types over `Bool` for domain logic.** `Bool` discards semantic information—callers must remember what `True` and `False` mean.

### The Problem

```haskell
-- AVOID: What does True mean?
isValid :: Input -> Bool
checkPermission :: User -> Resource -> Bool

-- PREFER: Self-documenting
data Validity = Valid | Invalid
validate :: Input -> Validity

data Permission = Granted | Denied
checkPermission :: User -> Resource -> Permission
```

**Problems with Bool:**
- Meaning requires documentation or function name inference
- Cannot attach additional error information
- Easy to accidentally negate logic

### Attach Information to Results

```haskell
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving stock (Eq, Ord, Show, Generic)

data AuthResult
  = Authorized
  | Unauthorized UnauthorizedReason
  deriving stock (Eq, Ord, Show, Generic)

data UnauthorizedReason = MissingPermission | QuotaExceeded
  deriving stock (Eq, Ord, Show, Generic)

-- Pattern matching is self-documenting
case authorize user resource of
  Authorized -> grantAccess
  Unauthorized MissingPermission -> showPermissionError
  Unauthorized QuotaExceeded -> showQuotaError
```

### When Bool Is Acceptable

**Use Bool for:**
- Primitive predicates: `(==)`, `(<)`, `null`, `even`
- Standard config flags: `verbose`, `debug`, `recursive`
- Type class methods: `Eq`, `Ord`

**Avoid Bool for:**
- Domain logic (validation, permissions, states)
- Business logic outcomes
- Anything where `True`/`False` meaning isn't immediately obvious

**References:** [Boolean Blindness - Existential Type](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/), [Medium](https://medium.com/@itsme.mittal/boolean-blindness-60937910e40e)

## Type Class Constraints

Never use redundant type-class constraints. Subclass constraints imply all superclasses across the entire hierarchy, including intermediate classes from packages like semigroupoids (e.g., `Monad` implies `Bind`, `Applicative`, `Apply`, and `Functor`; `Alternative` implies `Plus`, `Alt`, and `Applicative`; `Category` implies `Semigroupoid`; `Comonad` implies `Extend`; `Traversable1` implies `Foldable1`). Keep only the most specific constraint.

Never keep a constraint the function does not use, either, even to document an invariant. If an invariant matters, enforce it in the types (e.g. a smart constructor or a newtype) or document it in Haddock.

```haskell
-- WRONG: Functor is implied by Monad
f :: (Functor m, Monad m) => m a -> m b

-- WRONG: Ord is unused; it only "documents" that the input is sorted
fromSortedList :: Ord a => [a] -> SortedSet a
fromSortedList = coerce
```

`-Wredundant-constraints` reports both kinds and must stay enabled.

## Forbidden Instances

### Never Implement These Type Class Instances

**Never implement these instances** unless there is explicit, unambiguous, canonical behavior:

* `Read`, `Read1`, `Read2` — Parsing is ambiguous and fragile; use proper parser libraries
* `Enum` — Arbitrary ordering; rarely has canonical implementation
* `Bounded` — Arbitrary bounds; rarely meaningful

These instances create maintenance burden, break easily with refactoring, and rarely provide value. If serialization is needed, use `Binary` (binary), `Serialise` (serialise), or `ToJSON`/`FromJSON` (aeson).

### Never Use Boot Files (.hs-boot)

**NEVER use Haskell boot files (`.hs-boot`) to break module dependency cycles.**

Boot files are a GHC mechanism that allows mutual recursion between modules by providing type signatures separately from implementations. They create maintenance burden and indicate architectural problems.

**Why boot files are forbidden:**
- They duplicate type signatures, creating synchronization errors
- They break when refactoring (easy to forget to update both files)
- They indicate circular module dependencies (an architectural smell)
- They complicate build systems and IDE support
- They make code harder to understand and navigate

**When you encounter circular dependencies:**

1. **First choice: Redesign the module structure** — Extract shared types to a separate module
   ```haskell
   -- WRONG: A imports B, B imports A (circular)
   -- A.hs
   import B (TypeFromB)

   -- B.hs
   import A (TypeFromA)

   -- CORRECT: Extract shared types
   -- Types.hs
   module Types where
   data TypeFromA = ...
   data TypeFromB = ...

   -- A.hs
   import Types

   -- B.hs
   import Types
   ```

2. **Second choice: Merge modules** — If types are tightly coupled, they belong together
   ```haskell
   -- Instead of A.hs + B.hs with circular imports
   -- Use: Combined.hs with both definitions
   ```

3. **Third choice: Use type classes** — Abstract the dependency
   ```haskell
   -- Instead of concrete type imports causing cycles
   class HasFoo a where
     getFoo :: a -> Foo
   ```

**If you cannot resolve the circular dependency** using these approaches, **stop and inform the user**. Do not proceed with boot files. The architecture needs redesign.

### Absolute Prohibitions: Orphan and Overlapping Instances

**NEVER, under ANY circumstances, write orphan instances or overlapping instances.**

#### No Orphan Instances

An orphan instance is an instance declaration where:
- The type class is not defined in the current module, AND
- The data type is not defined in the current module

**Orphan instances break global uniqueness of instances and cause:**
- Compilation failures when modules are imported in different orders
- Runtime incoherence where different code sees different instances
- Import dependency nightmares where instance availability depends on unrelated imports
- Impossible-to-debug behavior where "identical" code behaves differently

**Wrong (orphan instance):**
```haskell
-- In module MyModule
import Data.Text (Text)
import Data.Aeson (ToJSON(..))

-- WRONG: Neither Text nor ToJSON is defined here
instance ToJSON Text where
  toJSON = ...
```

**Correct approaches:**

1. **Define instance where the type is defined:**
```haskell
-- In module MyType (where MyType is defined)
data MyType = MyType { field :: Int }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
```

2. **Use newtype wrapper:**
```haskell
-- In module MyModule
newtype MyText = MyText Text
  deriving newtype (Eq, Ord, Show, IsString)

instance ToJSON MyText where
  toJSON (MyText t) = toJSON t
```

3. **Contribute instance to upstream library:**
If a type needs an instance of a standard class, contribute it to the library that defines the type.

4. **Ask upstream to add deriving-via support:**
Modern libraries often provide newtype wrappers specifically for deriving instances.

#### No Overlapping Instances

Overlapping instances occur when multiple instance declarations could match the same type.

**NEVER use:**
- `{-# LANGUAGE OverlappingInstances #-}`
- `{-# LANGUAGE IncoherentInstances #-}`
- `instance {-# OVERLAPPING #-} ...`
- `instance {-# OVERLAPPABLE #-} ...`
- `instance {-# OVERLAPS #-} ...`

**Overlapping instances cause:**
- Unpredictable instance selection based on import order
- Compiler-dependent behavior across GHC versions
- Impossible-to-reason-about type class resolution
- Silent behavior changes when adding "unrelated" instances

**Wrong (overlapping instances):**
```haskell
-- WRONG: Overlapping instances
instance Show a => Show [a] where
  show = ...

instance {-# OVERLAPPING #-} Show [Char] where
  show = ...
```

**Correct approach (use newtypes):**
```haskell
-- Correct: Use distinct types
newtype CharList = CharList [Char]
  deriving newtype (Eq, Ord)

instance Show CharList where
  show (CharList cs) = ...

-- Or use existing types properly
-- String already has Show instance, no need for custom behavior
```

#### No Exceptions

There are no exceptions to these prohibitions — not for integration layers between two external libraries, not as a last resort. If an instance cannot be written without an orphan or an overlap, use a newtype wrapper or contribute the instance upstream. If neither is possible, **stop and inform the user**.

## Instance Implementation Guidelines

### Deriving Strategy Priority

**Always prefer deriving over manual instances:**

1. `deriving stock` — GHC built-in deriving
2. `deriving newtype` — Reuse underlying type's instances
3. `deriving via` — Derive through isomorphic type
4. `deriving anyclass` — Generic default implementations
5. Manual instances — Only when deriving impossible

```haskell
-- Regular data type
data MyType a b = MyType a b Int
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

-- Newtype: always use deriving newtype
newtype UserId = UserId Int
  deriving newtype (Eq, Ord, Show, Num, NFData)

-- Deriving via isomorphic types
newtype Score = Score Int
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype AppM a = AppM (ReaderT Config IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Config, MonadIO)
  deriving (Semigroup, Monoid) via (Ap AppM a)
```

### Write Manual Instances Only When Necessary

Write manual instances only when:
- Deriving is unavailable for the type class
- Deriving produces non-canonical behavior
- Performance requires specialized implementation
- The type is not a newtype and has no `Generic` instance

When deriving is unavailable or produces non-canonical behavior, write instances manually. Follow this implementation order (respecting dependencies):

1. Structural: `Generic`, `Generic1`, `Data`, `Typeable`, `NFData`
2. Equality/Ordering: `Eq`/`Eq1`/`Eq2`, then `Ord`/`Ord1`/`Ord2`
3. Display: `Show`/`Show1`/`Show2`
4. Algebraic: `Semigroup`, then `Monoid` (if identity exists)
5. Functor: `Functor` → `Apply` → `Applicative` → `Bind` → `Monad`
6. Alternative: `Alt` → `Plus` → `Alternative` → `MonadPlus`
7. Foldable/Traversable: After functor hierarchy established
8. Indexed: After base functor/foldable/traversable
9. Comonad: `Extend` → `Comonad`
10. Bifunctor: Similar progression to functor hierarchy
11. Profunctor: After bifunctor when applicable
12. Category/Arrow: After profunctor
13. Transformers: After monad hierarchy
14. Specialized: After foundational instances

### Lawfulness is Mandatory

**Do not implement instances that violate type class laws.** Common pitfalls:

- `Applicative` requires `pure` — impossible for types requiring unavailable context
- `Selective` requires `Applicative` superclass — cannot exist without lawful `pure`
- `Monad` must satisfy identity and associativity laws
- `Alternative` requires identity (`empty`) — only for types with true empty value
- `Comonad` requires `extract` — only when canonical extraction exists
- `Traversable` must visit each element exactly once in original order
- `Foldable1` requires non-empty structure — verify structure cannot be empty

When a lawful instance is impossible, **do not implement it**. Write a source comment explaining why if the impossibility is non-obvious.

### Verify Laws with Property Tests

For every instance of a lawful type class, write property-based tests verifying the laws using Hedgehog:

```haskell
-- | Verify Functor identity law
--
-- >>> check $ withTests 100 $ property $ do x <- forAll genMyType; fmap id x === x
--   ✓ <interactive> passed 100 tests.
-- True
prop_functorIdentity :: Property
prop_functorIdentity = property $ do
  x <- forAll genMyType
  fmap id x === x

-- | Verify Functor composition law
--
-- >>> check $ withTests 100 $ property $ do x <- forAll genMyType; fmap (f . g) x === fmap f (fmap g x)
--   ✓ <interactive> passed 100 tests.
-- True
prop_functorComposition :: Property
prop_functorComposition = property $ do
  x <- forAll genMyType
  let f = (+ 1)
      g = (* 2)
  fmap (f . g) x === fmap f (fmap g x)
```

Export law-checking functions for use by downstream libraries.

## Optics Type Classes Pattern

For every data type `XXX`, implement these optics type classes. Use `FunctionalDependencies` when the data type has type parameters.

### Getter Type Class

```haskell
-- | Type class for values that can be viewed as XXX
--
-- >>> view getXXX (myXXX :: XXX)
-- myXXX
class GetXXX s {- type params -} | s -> {- type params -} where
  getXXX :: Getter s (XXX {- type params -})

instance GetXXX (XXX {- type params -}) {- type params -} where
  getXXX = id
  {-# INLINE getXXX #-}
```

### Lens Type Class

```haskell
-- | Type class for values with a lens into XXX
--
-- >>> view xxx (myXXX :: XXX)
-- myXXX
-- >>> set xxx newValue myXXX
-- ...
class (GetXXX s {- type params -}) => HasXXX s {- type params -} | s -> {- type params -} where
  {-# MINIMAL setXXX #-}

  setXXX :: XXX {- type params -} -> s -> s

  xxx :: Lens' s (XXX {- type params -})
  xxx = lens (view getXXX) (flip setXXX)
  {-# INLINE xxx #-}

instance HasXXX (XXX {- type params -}) {- type params -} where
  setXXX = const
  {-# INLINE setXXX #-}
```

### Review Type Class

```haskell
-- | Type class for values that can be constructed from XXX
--
-- >>> review reviewXXX myXXX :: SomeType
-- ...
class ReviewXXX t {- type params -} | t -> {- type params -} where
  reviewXXX :: Review t (XXX {- type params -})

instance ReviewXXX (XXX {- type params -}) {- type params -} where
  reviewXXX = unto id
  {-# INLINE reviewXXX #-}
```

### Prism Type Class

```haskell
-- | Type class for values with a prism into XXX
--
-- >>> matchXXX (myXXX :: XXX)
-- Just myXXX
-- >>> preview _XXX someValue
-- ...
class (ReviewXXX t {- type params -}) => AsXXX t {- type params -} | t -> {- type params -} where
  {-# MINIMAL matchXXX #-}

  matchXXX :: t -> Maybe (XXX {- type params -})

  _XXX :: Prism' t (XXX {- type params -})
  _XXX = prism' (review reviewXXX) matchXXX
  {-# INLINE _XXX #-}

instance AsXXX (XXX {- type params -}) {- type params -} where
  matchXXX = Just
  {-# INLINE matchXXX #-}
```

## Writing Functions: Optics First

**Minimize writing top-level functions.** Before writing a function, verify it cannot be expressed using:

1. Type class methods (`fmap`, `traverse`, `fold`, etc.)
2. Lens combinators (`view`, `set`, `over`, `^.`, `.~`, `%~`, etc.)
3. Standard library functions (`maybe`, `either`, `bool`, etc.)
4. Composition of existing functions

When you must write a function, implement it using optics rather than pattern matching or field accessors:

**Prefer:**
```haskell
updateField :: Int -> MyType -> MyType
updateField n = fieldLens %~ (+ n)

getNestedValue :: MyType -> Int
getNestedValue = view (outerLens . innerLens . valueLens)
```

**Over:**
```haskell
updateField :: Int -> MyType -> MyType
updateField n (MyType a b c) = MyType a (b + n) c

getNestedValue :: MyType -> Int
getNestedValue (MyType _ (Inner _ (Value x)) _) = x
```

### Provide Both Traversal and Traversal1 When Applicable

For non-empty data structures, provide both variants:

```haskell
-- Traversal (may be empty, requires Applicative)
traverseValues :: Traversal' MyType Value

-- Traversal1 (guaranteed non-empty, only needs Apply)
traverseValues1 :: Traversal1' MyType Value
```

**When to provide Traversal1:** Data structure guarantees at least one element, enabling use with `Apply` instead of `Applicative`.

### Provide Both Fold and Fold1 When Applicable

```haskell
-- Fold (may be empty, requires Monoid)
foldValues :: Fold MyType Value

-- Fold1 (guaranteed non-empty, only needs Semigroup)
foldValues1 :: Fold1 MyType Value
```

**Summary:** For non-empty structures, provide both `Traversal`/`Traversal1` and `Fold`/`Fold1`. This enables weaker constraints (`Apply` vs `Applicative`, `Semigroup` vs `Monoid`).

## Documentation Requirements

### Haddock Documentation

```haskell
-- | Brief description
--
-- ==== __Examples__
--
-- >>> functionName arg1 arg2
-- expectedResult
--
-- @since X.Y.Z
functionName :: ...
```

Include doctests for: basic usage, edge cases, common patterns, law verification (Hedgehog).

### Doctest Setup Block

```haskell
-- $setup
-- >>> import Hedgehog
-- >>> import qualified Hedgehog.Gen as Gen
-- >>> let genInt = Gen.int (Range.linear (-100) 100)
```

Place after module header. Define imports and generators once for all doctests.

### Property Tests in Doctests

```haskell
-- | Functor instance
--
-- >>> check $ property $ do x <- forAll genMyType; fmap id x === x
--   ✓ passed 100 tests.
-- True
instance Functor MyType where
  fmap = ...
```

## Testing Requirements

### Test Suite Organization

Every project must have:

1. **Property test suite** — Verify type class laws for all instances (using Hedgehog)
2. **Doctest suite** — Verify all doctests execute correctly
3. **Benchmark suite** — Performance tests using criterion

### Property-Based Testing with Hedgehog

**Use Hedgehog exclusively. Do not use QuickCheck.**

Depend on `hedgehog` (https://hackage.haskell.org/package/hedgehog) for property-based testing.

For every type class instance:
- Test all laws defined by that type class
- Test with edge cases (empty, singleton, large inputs)
- Provide `Gen` instances for your data types

```haskell
genMyType :: Gen (MyType Int)
genMyType = MyType
  <$> Gen.int (Range.linear 0 100)
  <*> Gen.list (Range.linear 0 20) Gen.alpha
  <*> Gen.bool

prop_semigroupAssociativity :: Property
prop_semigroupAssociativity = property $ do
  x <- forAll genMyType
  y <- forAll genMyType
  z <- forAll genMyType
  (x <> y) <> z === x <> (y <> z)
```

#### Generating Functions with hedgehog-fn

**For testing higher-order functions, use `hedgehog-fn`** (https://hackage.haskell.org/package/hedgehog-fn) to generate random functions.

Depend on `hedgehog-fn` in your test suite when testing:
- Functor laws (requires generating functions)
- Applicative laws (requires generating functions)
- Monad laws (requires generating functions)
- Any higher-order function properties

```haskell
import Hedgehog.Function (Fn, applyFn, fn)
import qualified Hedgehog.Function as Fn

-- Generate functions for property tests
genIntToInt :: Gen (Fn Int Int)
genIntToInt = fn

genIntToBool :: Gen (Fn Int Bool)
genIntToBool = fn

-- Functor identity law
prop_functorIdentity :: Property
prop_functorIdentity = property $ do
  xs <- forAll genMyType
  fmap id xs === id xs

-- Functor composition law (requires function generation)
prop_functorComposition :: Property
prop_functorComposition = property $ do
  xs <- forAll genMyType
  f <- forAll genIntToInt
  g <- forAll genIntToInt
  fmap (applyFn f . applyFn g) xs === (fmap (applyFn f) . fmap (applyFn g)) xs

-- Applicative composition law
prop_applicativeComposition :: Property
prop_applicativeComposition = property $ do
  u <- forAll (genMyType genIntToInt)
  v <- forAll (genMyType genIntToInt)
  w <- forAll (genMyType Gen.int)
  let applyF = fmap applyFn
  pure (.) <*> applyF u <*> applyF v <*> w === applyF u <*> (applyF v <*> w)

-- Test a higher-order function
prop_mapPreservesLength :: Property
prop_mapPreservesLength = property $ do
  xs <- forAll (Gen.list (Range.linear 0 100) Gen.int)
  f <- forAll genIntToInt
  length (fmap (applyFn f) xs) === length xs

-- Test function with multiple arguments
genIntIntToBool :: Gen (Fn (Int, Int) Bool)
genIntIntToBool = fn

prop_filterProperty :: Property
prop_filterProperty = property $ do
  xs <- forAll (Gen.list (Range.linear 0 100) Gen.int)
  p <- forAll genIntToBool
  all (applyFn p) (filter (applyFn p) xs) === True
```

**Key points about hedgehog-fn:**

1. **Use `fn` to generate functions:**
   ```haskell
   genFunction :: Gen (Fn a b)
   genFunction = fn
   ```

2. **Apply generated functions with `applyFn`:**
   ```haskell
   f <- forAll genIntToInt
   result = applyFn f value
   ```

3. **Supports curried and uncurried functions:**
   ```haskell
   -- Curried (wrap in tuple)
   genCurried :: Gen (Fn (Int, Int) Int)
   genCurried = fn

   -- Use it
   f <- forAll genCurried
   result = applyFn f (x, y)
   ```

4. **Works with any testable type:**
   ```haskell
   -- Custom types (requires Arg instance, usually derivable)
   data MyType = MyType Int String
     deriving stock (Eq, Show, Generic)

   instance Arg MyType
   instance Vary MyType

   genMyTypeToInt :: Gen (Fn MyType Int)
   genMyTypeToInt = fn
   ```

5. **Essential for testing functor/applicative/monad laws:**
   - Cannot properly test composition without generating functions
   - `hedgehog-fn` ensures generated functions are deterministic and equality-testable
   - Functions are shown in test output when properties fail

**Always use hedgehog-fn when testing:**
- Functor instances (composition law)
- Applicative instances (composition and homomorphism laws)
- Monad instances (composition and associativity)
- Profunctor instances (require contravariant function generation)
- Any property involving higher-order functions (`map`, `traverse`, `foldMap`, etc.)

### Benchmark Suite with Criterion

```haskell
import Criterion.Main

main = defaultMain
  [ bgroup "operations"
      [ bench "mapping" $ whnf (fmap (+1)) largeData
      , bench "folding" $ whnf (foldl' (+) 0) largeData
      ]
  ]
```

## Performance Optimization

### Inline Pragmas: Use Liberally

**Add `INLINE` and `INLINABLE` pragmas liberally.** Inlining enables cross-module optimization and specialization.

**Priority order:**

1. **`{-# INLINE #-}`** — Default for most functions
   - Small functions (< 10 lines), type class methods, accessors, lens operations

2. **`{-# INLINABLE #-}`** — When INLINE isn't appropriate
   - Recursive functions, functions with constraints, medium-sized functions (10-30 lines)

3. **`{-# SPECIALIZE #-}`** — Add to INLINABLE for hot paths
   - Functions called frequently at specific types

4. **`{-# NOINLINE #-}`** — Rarely needed
   - Very large functions (> 50 lines)
   - Fusion workers use the phased form `{-# NOINLINE [1] #-}` instead (see [Fusion Rules](#fusion-rules-apply-aggressively))

**Examples:**

```haskell
-- Type class methods: always INLINE (the pragma goes inside the instance)
instance Functor MyType where
  fmap f (MyType x) = MyType (f x)
  {-# INLINE fmap #-}

-- Small functions: INLINE
{-# INLINE getField #-}
getField :: MyType -> Int
getField = view fieldLens

-- Recursive or constrained: INLINABLE
{-# INLINABLE traverseTree #-}
traverseTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseTree f (Leaf x) = Leaf <$> f x
traverseTree f (Branch l r) = Branch <$> traverseTree f l <*> traverseTree f r

-- Hot path: INLINABLE + SPECIALIZE
{-# INLINABLE processItems #-}
{-# SPECIALIZE processItems :: [Int] -> [Int] #-}
processItems :: Num a => [a] -> [a]
processItems = map (* 2) . filter (> 0)
```

**Rule of thumb:**
- `< 10 lines` → `INLINE`
- `Recursive/constraints` → `INLINABLE`
- Fusion workers (functions named in RULES) → `NOINLINE [1]`
- `> 50 lines` → Consider `NOINLINE` (rare)
- Type class methods & optics → Always `INLINE`

### Fusion Rules: Apply Aggressively

**Write RULES pragmas aggressively.** Fusion eliminates intermediate data structures, often yielding 10-100x performance improvements.

Without fusion: `map f . map g . map h` creates three intermediate lists.
With fusion: Single pass, no intermediate allocation.

#### Never Write Rules on Class Methods

**A rule whose left-hand side is a class method (`fmap`, `foldr`, `foldMap`, `traverse`, `bimap`, `first`, `mapMaybe`, ...) will not fire reliably.** GHC resolves the method to the instance's implementation first, and warns:

```
[-Winline-rule-shadowing] Rule "map/map" may never fire
  because rule "Class op fmap" for 'fmap' might fire first
```

Under `-Werror` this breaks the build. Do not write rules like `fmap f (fmap g xs) = ...`.

#### The Pattern: Rules on Worker Functions (as `base` does for lists)

`base` writes its list rules on `map` and `foldr`, not on `fmap`, and defines `fmap = map`. Follow the same pattern for every type:

1. **Write each operation as a top-level worker function** (`mapList`, `foldrList`, `mapMaybeList`, ...). This is the one sanctioned exception to [Optics First](#writing-functions-optics-first)'s "minimise top-level functions": rules need a named function to match.
2. **Mark recursive workers `{-# NOINLINE [1] #-}`.** They must not inline before their rules have had a chance to fire. This replaces `INLINABLE` for these functions.
3. **Define instance methods as the worker, marked `{-# INLINE #-}`.** `fmap` inlines to `mapList`, exposing it to the rules.
4. **Write rules on the workers, active before phase 1 (`[~1]`).**
5. **Non-recursive operations may be defined in terms of a worker and marked `INLINE`.** They then fuse through that worker's rules (below, `foldMap` and `traverse` fuse via `foldrList`).

```haskell
{-# LANGUAGE RankNTypes #-}

data List a = Nil | Cons a (List a)

-- Workers: recursive, NOINLINE [1]
mapList :: (a -> b) -> List a -> List b
mapList f = go
  where
    go Nil = Nil
    go (Cons x xs) = Cons (f x) (go xs)
{-# NOINLINE [1] mapList #-}

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList k z = go
  where
    go Nil = z
    go (Cons x xs) = k x (go xs)
{-# NOINLINE [1] foldrList #-}

mapMaybeList :: (a -> Maybe b) -> List a -> List b
mapMaybeList f = go
  where
    go Nil = Nil
    go (Cons x xs) = maybe (go xs) (`Cons` go xs) (f x)
{-# NOINLINE [1] mapMaybeList #-}

-- Non-recursive, defined via a worker: INLINE
foldMapList :: Monoid m => (a -> m) -> List a -> m
foldMapList f = foldrList (mappend . f) mempty
{-# INLINE foldMapList #-}

traverseList :: Applicative f => (a -> f b) -> List a -> f (List b)
traverseList f = foldrList (\x ys -> Cons <$> f x <*> ys) (pure Nil)
{-# INLINE traverseList #-}

-- Instance methods: the worker, INLINE
instance Functor List where
  fmap = mapList
  {-# INLINE fmap #-}

instance Foldable List where
  foldr = foldrList
  {-# INLINE foldr #-}
  foldMap = foldMapList
  {-# INLINE foldMap #-}

instance Traversable List where
  traverse = traverseList
  {-# INLINE traverse #-}

instance Filterable List where
  mapMaybe = mapMaybeList
  {-# INLINE mapMaybe #-}
```

#### Essential Fusion Rules

For every traversable data type, implement these categories, always on the workers:

**1. Functor Composition** (Functor composition law)
```haskell
{-# RULES
"mapList/mapList" [~1] forall f g xs.
  mapList f (mapList g xs) = mapList (f . g) xs
  #-}
```

**2. Map/Fold Fusion** (also fuses `foldMap` and `traverse` after a map, since both are defined via `foldrList`)
```haskell
{-# RULES
"foldrList/mapList" [~1] forall k z f xs.
  foldrList k z (mapList f xs) = foldrList (k . f) z xs
  #-}
```

**3. Filter Fusion** (Filterable composition law)
```haskell
{-# RULES
"mapMaybeList/mapMaybeList" [~1] forall f g xs.
  mapMaybeList f (mapMaybeList g xs) = mapMaybeList (\x -> g x >>= f) xs

"mapMaybeList/mapList" [~1] forall f g xs.
  mapMaybeList f (mapList g xs) = mapMaybeList (f . g) xs
  #-}
```

**4. Bifunctor Fusion**
```haskell
data P a b = P a b
  deriving stock (Functor)

bimapP :: (a -> c) -> (b -> d) -> P a b -> P c d
bimapP f g (P a b) = P (f a) (g b)
{-# NOINLINE [1] bimapP #-}

instance Bifunctor P where
  bimap = bimapP
  {-# INLINE bimap #-}

{-# RULES
"bimapP/bimapP" [~1] forall f1 f2 g1 g2 x.
  bimapP f1 g1 (bimapP f2 g2 x) = bimapP (f1 . f2) (g1 . g2) x
  #-}
```

**5. Build/Fold Fusion** (for list-like types)

Define the type's own `build`, marked `INLINE [1]` so it stays visible to the rule until phase 1:

```haskell
buildList :: (forall b. (a -> b -> b) -> b -> b) -> List a
buildList g = g Cons Nil
{-# INLINE [1] buildList #-}

{-# RULES
"foldrList/buildList" forall k z (g :: forall b. (a -> b -> b) -> b -> b).
  foldrList k z (buildList g) = g k z
  #-}
```

Never redefine `base`'s `build` or write rules on `base`'s functions; `base` already has its own.

#### Phase Control

```haskell
-- [2], [~1]: Composition and map/fold rules fire; workers are NOINLINE [1]
-- [1]:       Workers and buildList may now inline; rules marked [~1] switch off
-- [0]:       Final specialization
```

#### Verify Fusion

Check rule firings in a **client module**, one that imports the type and uses `fmap`, `foldr`, etc. Rules fire where the operations are composed:

```bash
ghc -O2 -ddump-rule-firings ClientModule.hs 2>&1 | grep "Rule fired"
ghc -O2 -ddump-simpl ClientModule.hs  # Check intermediate structures eliminated
```

**Write fusion rules for:** functor/foldable/traversable/filterable/bifunctor composition, build/fold patterns, coercion optimizations.

### Strictness Annotations

```haskell
data MyType = MyType
  { strictSmall :: !Int              -- Strict: small, always needed
  , lazyLarge   :: String             -- Lazy: large or seldom accessed
  , unpackStrict :: {-# UNPACK #-} !Int  -- Unpacked: remove indirection
  }
```

**Strict:** Small primitives, always-accessed fields, accumulators
**Lazy:** Large structures, seldom-accessed fields, potentially-bottom values

## Code Quality Standards

### Ignored Bindings and Pattern Variables

**Always use a plain underscore `_` for any unused binding — function arguments, lambda parameters, pattern-match components in `let`/`where`/`case`, `do`-block results, and anything else the body does not reference.**

Never use named wildcards like `_foo`, `_elemMerge`, `_vMerge`, or `_b`. If a name is not used, it must be a plain underscore with no suffix. This applies uniformly to *every* binding site, not just function arguments.

```haskell
-- WRONG: Named wildcard for unused function argument
someFunction _unusedArg x y = x + y

-- CORRECT: Plain underscore
someFunction _ x y = x + y

-- WRONG: Underscore prefix with name in lambda
mapMerge _vMerge = review _Merge' $ \_base deltasL deltasR -> ...

-- CORRECT: Plain underscore
mapMerge _ = review _Merge' $ \_ deltasL deltasR -> ...

-- WRONG: Named wildcard in let-pattern
let (a, _b) = splitPair x
in useOnly a

-- CORRECT: Plain underscore in let-pattern
let (a, _) = splitPair x
in useOnly a

-- WRONG: Named wildcard in where-pattern
firstOf xs = a
  where (a, _rest) = uncons xs

-- CORRECT: Plain underscore in where-pattern
firstOf xs = a
  where (a, _) = uncons xs

-- WRONG: Named wildcard in case-pattern
case parseResult of
  Left _err  -> defaultValue
  Right v    -> v

-- CORRECT: Plain underscore in case-pattern
case parseResult of
  Left _  -> defaultValue
  Right v -> v

-- WRONG: Named wildcard for ignored do-block result
do
  _result <- someEffect
  pure ()

-- CORRECT: Plain underscore (or use `void`)
do
  _ <- someEffect
  pure ()
```

There are no exceptions. If a comment needs to explain an unused argument, describe it by position or type, and still bind it as `_`.

This rule prevents "warning-suppression drift" where underscore prefixes are added purely to silence warnings but make the code less clear. Every unused name should be a genuine `_`, so the compiler's unused-binding warnings remain a reliable signal.

### Compiler Warnings: Mandatory in Every File

**Every Haskell source file must include** `{-# OPTIONS_GHC -Wall #-}` at the top.

This is **non-negotiable**. Place it immediately after language extensions, before the module declaration:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Data.MyModule where
```

#### Additional Warning Flags

```haskell
{-# OPTIONS_GHC -Wall -Wcompat -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wredundant-constraints #-}
```

Build with `-Werror` in CI.

**Why -Wall:** Catches unused imports, incomplete patterns, type defaulting, shadowing, missing signatures, tabs, overlapping patterns.

#### Forbidden Warning Suppressions

**NEVER suppress these warnings under any circumstances:**

```haskell
-- ABSOLUTELY FORBIDDEN - DO NOT USE:
{-# OPTIONS_GHC -Wno-orphans #-}               -- Hides orphan instances (forbidden)
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}  -- Hides overlapping instances (forbidden)
{-# OPTIONS_GHC -fno-warn-orphans #-}          -- Old syntax, still forbidden
{-# OPTIONS_GHC -Wno-unused-top-binds #-}      -- Hides unused top-level bindings (forbidden)
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- Old syntax, still forbidden
{-# OPTIONS_GHC -Wno-unused-matches #-}        -- Hides unused pattern variables (forbidden)
{-# OPTIONS_GHC -Wno-unused-local-binds #-}    -- Hides unused let/where bindings (forbidden)
{-# OPTIONS_GHC -Wno-unused-binds #-}          -- Hides all unused bindings (forbidden)
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- Hides redundant constraints (forbidden)
```

The unused-binding warnings must remain active on every module. If a top-level binding, pattern variable, or local binding is unused, the fix is to **rename it to `_`** (for pattern variables) or **remove it** (for top-level bindings) — never to suppress the warning. Named wildcards like `_foo` also *silence* the unused-matches warning, which is why the [Ignored Bindings and Pattern Variables](#ignored-bindings-and-pattern-variables) rule forbids them: keeping the warning enabled is only useful if every unused name is a genuine `_`.

**NEVER enable these dangerous language extensions:**

```haskell
-- ABSOLUTELY FORBIDDEN - DO NOT USE:
{-# LANGUAGE OverlappingInstances #-}      -- Creates instance incoherence (deprecated)
{-# LANGUAGE IncoherentInstances #-}       -- Even worse than overlapping (deprecated)
```

If you encounter one of these warnings or think you need these extensions:
1. You are doing something fundamentally wrong
2. Redesign your approach using newtypes, type families, or other safe mechanisms
3. The code cannot be written correctly with these features enabled

`UndecidableInstances` is not in this list. It is allowed when accompanied by a documented reason — see [Extensions Requiring Documentation](#extensions-requiring-documentation).


### Linting with HLint

**Run `hlint` on all source files before every commit.** HLint catches common mistakes and suggests idiomatic improvements.

#### Configure HLint

```yaml
# .hlint.yaml
- arguments: [--color=auto, --cpp-simple]
- ignore: {name: "Redundant bracket"}  # Brackets aid clarity
- error: {name: "Use camelCase"}
```

#### HLint Compliance

Address all suggestions unless:
1. Testing type class laws (intentionally writing both sides)
2. Project style conflict (document in `.hlint.yaml`)
3. Reduces clarity (document inline with `{-# HLINT ignore #-}`)

Never ignore: redundant lambdas, unused imports, boolean simplifications.

#### HLint in CI

Add HLint check to continuous integration:

```bash
# In CI script
hlint src/ test/ bench/ --fail
```

This fails the build if HLint finds issues, ensuring code quality.

### Formatting

Use **ormolu**, **fourmolu**, or **stylish-haskell**. Format before committing.

## Pre-Commit Checklist

Before committing code:

1. ✓ Run formatter (ormolu/fourmolu/stylish-haskell)
2. ✓ Run linter (hlint) and address all issues
3. ✓ Build with `-Wall -Werror` (warnings as errors)
4. ✓ Run all test suites:
   - Property tests (verify all laws)
   - Doctests (verify all examples)
   - Unit tests (if any)
5. ✓ Run benchmarks and verify no performance regressions
6. ✓ Build Haddock documentation and review locally
7. ✓ Build from source distribution (`cabal sdist` then build tarball)

## Version Management

Follow PVP:
- `A.0.0.0` — Breaking changes
- `0.A.0.0` — Compatible additions
- `0.0.A.0` — Bug fixes
- `0.0.0.A` — Bounds/metadata

Document in `changelog.md`.

## Language Extensions

### Recommended Extensions

These extensions are safe and commonly useful in modern Haskell:

```haskell
{-# LANGUAGE DeriveGeneric #-}              -- Enable Generic deriving
{-# LANGUAGE DerivingStrategies #-}         -- Explicit deriving strategies
{-# LANGUAGE DerivingVia #-}                -- Derive via isomorphic types
{-# LANGUAGE FlexibleContexts #-}           -- More flexible instance contexts
{-# LANGUAGE FlexibleInstances #-}          -- More flexible instance heads
{-# LANGUAGE FunctionalDependencies #-}     -- Multi-param type classes with FunDeps
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Newtype deriving
{-# LANGUAGE ScopedTypeVariables #-}        -- Scoped type variables
{-# LANGUAGE TypeFamilies #-}               -- Type families for associated types
{-# LANGUAGE StandaloneDeriving #-}         -- Deriving outside data declaration
{-# LANGUAGE InstanceSigs #-}               -- Type signatures in instances (documentation)
{-# LANGUAGE MultiParamTypeClasses #-}      -- Multiple type parameters (use with FunctionalDependencies)
{-# LANGUAGE RankNTypes #-}                 -- Higher-rank polymorphism (essential for lens, exists, ST)
{-# LANGUAGE GADTs #-}                      -- Generalized algebraic data types (precise typing)
{-# LANGUAGE TypeOperators #-}              -- Type-level operators (->>, :~:, etc.)
{-# LANGUAGE DataKinds #-}                  -- Promote data constructors to type level (standard)
{-# LANGUAGE TypeApplications #-}           -- Explicit type application with @ (very useful)
{-# LANGUAGE PolyKinds #-}                  -- Kind polymorphism (often enabled automatically)
{-# LANGUAGE ConstraintKinds #-}            -- First-class constraints (constraint synonyms)
```

**These are all standard extensions in modern Haskell.** Use them freely when they make code clearer or solve the problem at hand.

### Extensions Requiring Documentation

Use these only when necessary, and **document why** with an inline comment:

```haskell
{-# LANGUAGE UndecidableInstances #-}  -- Makes type checking non-terminating (document why safe)
```

**Why this needs documentation:**
- **UndecidableInstances**: Makes type checking potentially non-terminating. Safe for simple wrapper instances, structurally decreasing instances, and terminating type-level computation, but document the reasoning (see [UndecidableInstances: Use When Necessary](#undecidableinstances-use-when-necessary)).

### Extensions to Avoid

**Never use, under any circumstances:**

```haskell
{-# LANGUAGE OverlappingInstances #-}      -- FORBIDDEN: Use newtypes instead
{-# LANGUAGE IncoherentInstances #-}       -- FORBIDDEN: Breaks coherence completely
```

**Never use** without extremely strong justification:

```haskell
{-# LANGUAGE ImpredicativeTypes #-}        -- Almost never works correctly
{-# LANGUAGE AllowAmbiguousTypes #-}       -- Usually indicates design problem
{-# LANGUAGE PartialTypeSignatures #-}     -- Defeats type safety
```

### NoImplicitPrelude Pattern

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
import Control.Applicative (Applicative(..))
```

Use for custom prelude libraries or explicit import control. Avoid in regular application code.

## Common Patterns and Idioms

### The GetX/HasX/ReviewX/AsX Optics Pattern

This is a standard pattern observed across all reviewed projects. For every data type `X` (with type parameters, add them to each class with functional dependencies, as in [Optics Type Classes Pattern](#optics-type-classes-pattern)):

```haskell
-- 1. Getter type class (most general - read-only view)
class GetX s where
  getX :: Getter s X

instance GetX X where
  getX = id

-- 2. Lens type class (read-write access); instances define setX
class GetX s => HasX s where
  {-# MINIMAL setX #-}
  setX :: X -> s -> s
  x :: Lens' s X
  x = lens (view getX) (flip setX)

instance HasX X where
  setX = const

-- 3. Review type class (construct values)
class ReviewX t where
  reviewX :: Review t X

instance ReviewX X where
  reviewX = unto id

-- 4. Prism type class (partial matching); instances define matchX
class ReviewX t => AsX t where
  {-# MINIMAL matchX #-}
  matchX :: t -> Maybe X
  _X :: Prism' t X
  _X = prism' (review reviewX) matchX

instance AsX X where
  matchX = Just
```

Instances define `setX` and `matchX`, which are plain functions; the lens `x` and the prism `_X` follow from them and from the `GetX` and `ReviewX` superclasses.

**Always provide all four** for every data type. This enables:
- Maximum polymorphism (functions work on any type with the optics)
- Composition with other optics
- Integration with lens ecosystem

### Wrapper Types Pattern

For newtype wrappers around functors:

```haskell
newtype Wrap f a = Wrap (f a)
  deriving newtype (Eq, Ord, Show, Functor, Foldable)
  -- Traversable cannot be newtype-derived (role restriction); use stock
  deriving stock (Traversable, Generic, Generic1)

-- Wrapped/Rewrapped for lens integration
instance Wrapped (Wrap f a) where
  type Unwrapped (Wrap f a) = f a
  _Wrapped' = iso (\(Wrap x) -> x) Wrap
  {-# INLINE _Wrapped' #-}

instance (t ~ Wrap g b) => Rewrapped (Wrap f a) t
```

Do not write `unWrap`/`mapWrap`-style functions. Use the `Wrapped`/`Rewrapped` optics:

```haskell
view _Wrapped' w             -- unwrap:  Wrap f a -> f a
review _Wrapped' x           -- wrap:    f a -> Wrap f a
over _Wrapped f w            -- map:     (f a -> g b) -> Wrap f a -> Wrap g b
```

### Data Types with Multiple Component Pattern

For data types with multiple components representing different cases:

```haskell
data Result f g a b = Result
  { successes :: f (a, b)        -- Successful pairings
  , failures :: Maybe (Either (g a) (g b))  -- Failed elements (left or right)
  }

-- Common alias for same functor
type Result' f a b = Result f f a b
```

This pattern enables representing results where some elements succeed and others fail.

## Summary

Write **type class-driven, optics-based, law-abiding Haskell**:

1. **Implement all canonical instances** — Maximize integration with ecosystem
2. **Verify all laws** — Use Hedgehog property tests, export law-checking functions
3. **Prefer optics** — Use lens over pattern matching and field accessors
4. **Document comprehensively** — Haddock, doctests with examples and properties
5. **Test thoroughly** — Properties, doctests, benchmarks
6. **Maintain quality** — Format, lint, address warnings, verify laws
7. **Optimize aggressively** — INLINE/INLINABLE liberally, write fusion RULES
8. **Provide convenience** — Type aliases, optics type classes, helper functions
9. **Be rigorous** — Lawfulness, performance, documentation, testing
10. **Follow patterns** — Use established patterns from quality libraries
