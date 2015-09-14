---
layout: post
title: Function sensitivity in Idris
---

Imagine a type-system capable of more than just checking the boundaries of function applications.
Imagine it informing you that your sort doesn't sort, your map is dropping elements, and that no, you can't take the head of an empty list.
Per Martin-Loef's intuitionistic types (a.k.a. dependent types) provide a powerful framework for recording and verifying our assumptions about code.

Of course, with great power comes great complexity.. or was it responsibility, or..
Point is: dependent types are hard.
But a little bit of understanding can go a long way.

In this post, I hope to illustrate how dependent types can be used to capture the concept of function sensitivity.
Code snippets will be presented in Idris [XX].

#### What is function sensitivity?

A function's sensitivity represents how "far" it can magnify distances between pairs of inputs.
Specifically, it measures the maximum multiplicative factor by which the distance could increase.
We say that a function is c-sensitive if, for all pairs of inputs, the distance between the outputs is not \\(c\\) times longer than the distance between the inputs.

$$ \forall x,y. d(f(x),f(y)) \le c \times d(x,y) $$

Imagine that we have a function mapping points on a 2D plane to somewhere else on that 2D plane.
Consider a pair of points on the plane and draw a line between them.
Now imagine that we apply the function to each of the points and draw a line between the new points.
The second line can, at most, be \\(c\\) times longer than the first line; where \\(c\\) is some constant.

[visualisation of prev. paragraph here]

#### A simple example

For a simple example, consider a function adding 10 to any real number.

```idris
add10 : Float -> Float
add10 = (+10)
```

Now consider a pair of possible inputs.
For the sake of example, let us pick the numbers four and eight, and take the distance function to be Euclidian.
The distance between them is 4 (`d = abs(4-8)`).
Applying `add10` to the inputs and computing the distance between the outputs also yields 4. 
Thus, we can conclude that it is c-sensitive for all \\(c \ge 1\\).
We are primarily interested in a lower bound on \\(c\\), so we say that `add10` is 1-sensitive.

Contrast `add10` with a function that doubles it's input.

```idris
double : Float -> Float
double = (*2)
```

It should be clear to see that `double` is a 2-sensitive function.
Try it with a few arbitrary input pairs if you aren't convinced.

Also, any function that is c-sensitive is also c'-sensitive, for all \\(c < c'\\).

##### Exercises

* what is the sensitivity of `sin(x)`?
* is a function \\(f : \mathbb R \rightarrow [0,1]\\) necessarily 1-sensitive?

#### Sensitive function composition

Computing the sensitivity of each function manually is both tedious and error-prone.
Especially if we are building our computations up from smaller functions for which we already have sensitivities, such as `add10` and `double`.
So how do sensitivities compose?

If we play with the numbers a bit, we will find that the composition `add10 . double` is 2-sensitive.
So is `double . add10`.
What about `double . double`?
It is 4-sensitive.
Sensitivities are composed by multiplying them.
(Go ahead. Try it out if you don't believe me!)

#### Typing sensitivity

We want our type system to be function sensitivity aware.
So we wrap a regular function behind a data constructor and allow the type to be indexed by values from \\(\mathbb R\\).

```idris
data SensitiveFunction : Float -> Type -> Type -> Type where
  MkSensitiveFunction : (a -> b) -> SensitiveFunction s a b
```

Now we can rewrite `add10` and `double` to reflect their sensitivities.

```idris
add10 : SensitiveFunction 1 Float Float
add10 = MkSensitiveFunction (+10)
double : SensitiveFunction 2 Float Float
double = MkSensitiveFunction (*2)
```

Of course, functions need to be applicable and composable.

```idris
app : SensitiveFunction s a b -> a -> b
app (MkSensitiveFunction f) x = f x

(.) : SensitiveFunction s b c -> SensitiveFunction s' a b -> SensitiveFunction (s*s') a c
(.) (MkSensitiveFunction f) (MkSensitiveFunction g) = f . g
```







