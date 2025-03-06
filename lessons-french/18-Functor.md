Functor

Here is the English translation of your text:

---

# Functor  

Welcome to a new lesson of the Haskell course. This one is all about the **Functor** type class.  

### **Functors are 🌏 EVERYWHERE!! 🌎**  

In this lesson:  
- You'll understand the concept of **Functor**.  
- You'll learn everything you need to know to use them in practice.  

We have functors in mathematics, programming languages, linguistics, and even under your bed waiting for you to go to sleep... **Functors are everywhere!!** You might have worked with functors without even knowing.  

After this lesson, you will not only understand the concept of **Functor** in Haskell, but we'll also go the extra mile so you can **learn everything you need to know to actually use it in practice**.  

And how are we going to do that? This is how:  

---

## **Outline**  
- Abstracting the `map` function  
- The **Functor** type class  
- Defining **Functor** instances  
- Seemingly unintuitive **Functor** instances  
- The `Either a` functor 🤔  
- The `(,) a` functor 🤨  
- The `(->) r` functor 🤯  
- Defining `<$>` and **Lifting** 🏋️ a function  
- **Functor nesting dolls** 🪆  
- Extra functions and **Functor** as defined in `base`  

---

## **Abstracting the `map` function**  

This chapter will be super easy since you already know about `map`. Let's implement a function that returns the lowercase version of a `String`:  

```haskell
import Data.Char (toLower)

lowerString :: [Char] -> [Char]
lowerString []     = []
lowerString (x:xs) = toLower x : lowerString xs

lowerString "Hi, How's it Going?"
-- "hi, how's it going?"
```

Now, let's implement a function that adds one to a list of numbers:  

```haskell
addOne :: Num a => [a] -> [a]
addOne []     = []
addOne (x:xs) = (x + 1) : addOne xs

addOne [1,1,2,3]
-- [2,2,3,4]
```

And now, let's implement a function that transforms a list of boolean values into a list of characters representing bits:  

```haskell
boolToBit :: [Bool] -> [Char]
boolToBit []     = []
boolToBit (x:xs) = (if x then '1' else '0') : boolToBit xs

boolToBit [True,False,True]
-- "101"
```

Okay. So, I'm sure you see where I'm going with this. There's a repeating pattern, so we'll **extract it into its own function**.  

Let's start with the type. As input, we have:  
- A **list of characters**  
- A **list of numbers**  
- A **list of booleans**  

So, the most **general** type would be `[a]` (**a list of `a`**) for the input list.  

For the **output type**, at first glance, we could use the same `[a]`, but as we see in the `boolToBit` function, sometimes the output type is different. So, let's **use a different type variable**, like `[b]`:  

```haskell
map :: [a] -> [b]
```

Now, let's extract the pattern:  

```haskell
map []     = []
map (x:xs) = f x : map xs
```

This looks mostly okay, but we have to **get the function `f` from somewhere**, so we add it as a parameter.  

Since the function `f` takes a value of type `a` and produces a value of type `b`, its type is `a -> b`. So, the final expression of our abstraction looks like this:  

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

Now, we can use `map` like this:  

```haskell
map toLower "Hi, How's it Going?"
-- "hi, how's it going?"

map (+1) [1,1,2,3]
-- [2,2,3,4]

map (\x -> if x then '1' else '0') [True,False,True]
-- "101"
```

Awesome! **We abstracted away** the concept of applying a function to each element of a list, and we called this abstraction **map**. Then, we used it to avoid repeating ourselves and simplify our code.  

This is cool, but we can **do even better**. Let's **go one level higher** with the **Functor** type class.

---

## **Abstracting the Functor Type Class**  

In Haskell, we have **many types**. Let's say we're working with **optional values** using the `Maybe` type. Just like with lists, we also need a way to **modify values inside `Maybe` types**.  

No big deal, we know the drill. We can define the `maybeMap` function:  

```haskell
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just x) = Just (f x)
```

Now, let's use `maybeMap`:  

```haskell
maybeMap toLower (Just 'A')   -- Just 'a'
maybeMap (+1) (Just 3)        -- Just 4
maybeMap (\x -> if x then '1' else '0') (Just True)  -- Just '1'

maybeMap toLower Nothing  -- Nothing
maybeMap (+1) Nothing     -- Nothing
maybeMap (\x -> if x then '1' else '0') Nothing  -- Nothing
```

As you can see, `maybeMap` **can't modify** a `Nothing` value—it just **propagates the error**. But when we **have a value**, we apply the function and **wrap it again** in `Just`.  

Notice how the function we apply as the first parameter in both `map` and `maybeMap` **doesn't care about the structure**—it **only modifies the values**.  

---

## **Defining the Functor Type Class**  

Now that we've seen how `map` and `maybeMap` work, we can **generalize this pattern** into a type class called **Functor**:  

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

### **Functor Laws**  
When defining instances of `Functor`, we must follow two fundamental laws:  

#### **Identity Law**  
Applying `fmap id` should be the same as applying `id` directly to the structure:  

```haskell
fmap id == id
```

#### **Composition Law**  
Applying `fmap` to a function composition should be the same as **composing `fmap` calls**:  

```haskell
fmap (f . g) == fmap f . fmap g
```

---

## **Defining Functor Instances**  

Now, let's define some `Functor` instances:  

### **Lists**  
```haskell
instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs
```

### **Maybe**  
```haskell
instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

### **Binary Trees**  
```haskell
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x)       = Leaf (f x)
  fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)
```

---

And that’s it! Now you **understand Functors** and how to **use them in practice**.  

🎉 **Congratulations!** 🎉

Here's the English translation of the given Haskell explanation:  

---

This might blow your mind, but I have to say it... **functions are functors!** Hear me out.  

### Understanding Type Constructors  

In Haskell, we can inspect the kinds of different types:  

```haskell
:k (Char, Bool)
-- (Char, Bool) :: *

:k (,)
-- (,) :: * -> * -> *
```

Now, let's define a function:  

```haskell
biggerThan3 :: Int -> Bool
biggerThan3 = (>3)
```

Checking its type:  

```haskell
:t biggerThan3
-- biggerThan3 :: Int -> Bool

:k (Int -> Bool)
-- (Int -> Bool) :: *
```

Similarly, for the function arrow `->`:  

```haskell
:k (->)
-- (->) :: * -> * -> *
```

So, just like the **pair type constructor `(,)`**, which takes two types and creates a tuple type, the **function type constructor `(->)`** takes two types and creates a function type.  

For example:  

- `(,) Char Bool` gives us the type `(Char, Bool)`, a tuple of a character and a boolean.  
- `(->) Int Bool` gives us `Int -> Bool`, a function that takes an `Int` and returns a `Bool`.  

### Making `(->) r` a Functor  

Just like we partially apply `(,)` to create a functor instance:  

```haskell
:k (,)
-- ❌ Not a functor by itself

:k (,) Int
-- ✅ Now it has the right kind to be a functor: * -> *
```

We can do the same for `(->)`:  

```haskell
:k (->)
-- ❌ Not a functor by itself

:k (->) Int
-- ✅ Now it has the right kind to be a functor: * -> *
```

So, let's define the `Functor` instance for functions:  

```haskell
instance Functor ((->) r) where
  ...
```

Here, `r` is the first type applied to `(->)`, meaning it’s the input type of the function.  

### Understanding `fmap` for Functions  

The type of `fmap` is:  

```haskell
fmap :: (a -> b) -> f a -> f b
```

For our special case of `(->) r`:  

```haskell
fmap :: (a -> b) -> (->) r a -> (->) r b
```

Haskell's syntactic sugar means we usually write `(->) r a` as `r -> a`:  

```haskell
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

That means `fmap` takes a function `(a -> b)` and another function `(r -> a)` and produces a function `(r -> b)`.  

### Implementing `fmap` for Functions  

Both arguments are functions, so let’s call them `f` and `g`:  

```haskell
instance Functor ((->) r) where
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap f g = ...
```

Since we need a function that takes `r` and returns `b`, and we have:  

- `g :: r -> a`
- `f :: a -> b`

We just **compose** them:  

```haskell
instance Functor ((->) r) where
  fmap f g = f . g
```

Since `fmap f g` is just `f . g`, we can write it even more succinctly:  

```haskell
instance Functor ((->) r) where
  fmap = (.)
```

That’s it! Function composition is `fmap` for functions.  

### Testing It  

Let’s test it with some values:  

```haskell
value1 :: Int -> Int
value1 = (*2)

:t value1
-- value1 :: Int -> Int

value1 3
-- 6

:t fmap (>4) value1
-- fmap (>4) value1 :: Int -> Bool

(fmap (>4) value1) 3
-- True
```

Another example:  

```haskell
value2 :: Bool -> Char
value2 x = if x then '1' else '0'

:t value2
-- value2 :: Bool -> Char

value2 True
-- '1'

:t fmap succ value2
-- fmap succ value2 :: Bool -> Char

(fmap succ value2) True
-- '2'
```

Even the **Functor laws** hold:  

```haskell
(fmap id value1) 5 == (id value1) 5
-- True

(fmap id value2) False == (id value2) False
-- True
```

It works! 🎉  

### `<$>` and Function Lifting  

Since `fmap` is binary, we can use it infix-style:  

```haskell
fmap (+1) (Just 3)
-- Just 4

(+1) `fmap` (Just 3)
-- Just 4
```

Haskell provides `<$>` as an infix synonym for `fmap`:  

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

So, we can rewrite:  

```haskell
(+1) <$> Just 3
-- Just 4
```

### `<$>` vs `$`  

The operator `<$>` is similar to `$`, but it works on functors:  

```haskell
($)  ::              (a -> b) ->   a ->   b
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

Visually:  

```haskell
($)  ::              (a -> b) -> (  a ->   b)
(<$>) :: Functor f => (a -> b) -> (f a -> f b)
```

This means `<$>` **lifts** a function to work on functor values.  

For example:  

```haskell
:t toLower
-- toLower :: Char -> Char

:t (toLower <$>)
-- (<$>) toLower :: Functor f => f Char -> f Char
```

### Working with Nested Functors 🪆  

Functors can be nested, like:  

```haskell
value1 :: Either String Bool
value1 = Right False

value2 :: [Either String Bool]
value2 = [Left "error", Right True, Right False]
```

To apply `fmap` at multiple levels, use function composition:  

```haskell
(fmap . fmap) boolToBit value2
-- [Left "error", Right '1', Right '0']
```

For deeper levels:  

```haskell
(fmap . fmap . fmap) boolToBit value3
(fmap . fmap . fmap . fmap) boolToBit value4
```

### Extra Functor Functions  

- **Flipped fmap**:  

```haskell
(<&>) :: Functor f => f a -> (a -> b) -> f b 
as <&> f = f <$> as
```

Example:  

```haskell
[1,2,3] <&> (+1)
-- [2,3,4]
```

- **Replace values in functors**:  

```haskell
(<$) :: a -> f b -> f a
(<$) = fmap . const
```

Example:  

```haskell
'a' <$ Just 2
-- Just 'a'
```

- **Discard values with `void`**:  

```haskell
void :: Functor f => f a -> f ()
void x = () <$ x
```

Example:  

```haskell
void [1,2,3]
-- [(),(),()]
```

### Conclusion  

Functions are functors, and `fmap` for functions is just **function composition**. Once you grasp this, it opens the door to deeper functional programming concepts like Applicatives and Monads. 🚀
