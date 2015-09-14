---
layout: post
title:  "Chapter XX: Power of Pi - Dependently Typing Database Schemas"
date:   2015-08-18 13:43:22
---

In this chapter, we demonstrate how dependent types can be used to type the relational algebra.
Our approach is based on the paper _Power of Pi_ [XX].

# What is the relational algebra?

<!-- Introduction to relational databases -->
The relational algebra describes semantics for modelling and querying data in relational databases (e.g. MySQL).
E.F. Codd described relational databases and their associated algebra back in the 1970's [XX].
The relational model quickly became popular for its simplicity and good performance.

<!-- What is a relational database -->
In the relational algebra, relations are modelled as tables.
The rows of a table represent individual entities and the columns represent their attributes.
In most implementations, each column is required to have a specific type.
Every table has a _schema_, which is the list of it's column names and types.
For example, the table below contains four columns; each has a unique name and an associated data type.

<!-- Figure XX -->

Key     |   Name |     Age | Favorite Food
Integer | String | Integer | String
------- | ------ | ------- | -------------
      1 | Casper |      26 |    Bruschetta
      2 |  Gismo |       2 |    Dog Food

<!-- What is relational algebra -->

The relational algebra allows us to build structured queries against relational databases.
The primary operators are listed below.
Note that some of the operators may require expressions to be provided (e.g. for filtering a table with a conditional).

* selection
* projection
* Cartesian product*
* set union*
* set difference*

\* These operations function identically to their set counterparts, but impose additional requirements regarding table schemas.
In particular, union and difference (and therefore intersection) require that the schemas of the two operands match.
The Cartesian product operator requires that the two schemas are disjoint.

# Typing the relational algebra

<!-- Introducing dependent types in this context -->

The additional requirements for set operations have precluded many type systems from being able to fully type-check the relational algebra.
Strongly-typed languages such as Haskell have devised clever solutions to this problem [XX maybe Ken's paper], but none are as straight-forward as the approach made available with dependent types.
The ability to capture schemas directly in the types and to manipulate them yields a deceptively simple, but powerful typing framework for relational databases.

This section will briefly outline a simplified version of our implementation of the relational algebra.

<!-- How we represent it in Idris -->

### Expressions

<!-- Description of:
     src/Database/PowerOfPi/Types.idr -->

To start with, we need a way to represent table attributes (columns).
An attribute is just the pairing of a name to a data type.
Our implementation uses `(:::)` instead of the traditional `(,)` to make schemas more readable.
A schema is just a list of attributes.

{% highlight idris %}
-- src/Database/PowerOfPi/Abstract/Types.idr
data Attribute : Type where
  (:::) : String -> Type -> Attribute

Schema : Type
Schema = List Attribute
{% endhighlight %}

Already now we are able to abstractly describe relational database tables.
For instance, the schema of the example table from earlier would be described in Idris as follows.

{% highlight idris %}
[ "Key"           ::: Integer
, "Name"          ::: String
, "Age"           ::: Integer
, "Favorite Food" ::: String
] : Schema
{% endhighlight %}

<!-- Description of:
     src/Database/PowerOfPi/Expr.idr -->

The relational algebra assumes an expression language, so we provide a primitive example of one.
An expression of the form `Expr s a` can be read as an expression running against schema `s` returning type `a`.
Our expression language is far from complete; the goal was to demonstrate a working expression language, not reinvent functional programming.
However, the `PureFn` data constructor actually lifts pure Idris functions into the expression language, which should allow advanced programmers to bend the expression language to the task at hand.
[XX We still need to research how dangerous including PureFn is]

{% highlight idris %}
-- src/Database/PowerOfPi/Abstract/Expr.idr
data Expr : (s:Schema) -> (t:Type) -> Type where
  (^) : (s:Schema) -> (nm:String) -> { auto p : (map cast s) `ContainsKey` nm } -> Expr s (lookupType s p)
  (+) : Num t  => Expr s t -> Expr s t -> Expr s t
  (==): Eq t   => Expr s t -> Expr s t -> Expr s Bool
  Lit : Show t => (val:t) -> Expr s t
  PureFn : (a -> b) -> Expr s a -> Expr s b
  Couple : Expr s t -> Expr s t' -> Expr s $ Pair t t'
{% endhighlight %}

N.B. `ContainsKey` is a proof that the given schema does indeed contain a column with the given name.
It is derived automatically, so users should never have to worry about it.
`lookupType` uses this (derived) proof to return the type of the column.

Of course, expressions are built up from sub-expressions, usually using binary operators.
The result is an expression tree.
Our expression language only allows well-typed expressions to be constructed.

What is the "type" of a complex expression tree?
Expressions can't change the schema; only the return type.
Therefore, the type of an expression tree must be the return type of the top-most node.

![dingdong]({{site.img}}/expr_tree.png)

<!-- Description of:
     src/Database/PowerOfPi/Query.idr -->

### Queries

The core of the relational algebra consists of the query operators.
They are modelled as follows.
Note that `Query` is a _data family_ to help us abstract away the backend; i.e. we use an injective function, `TableType`, to determine whether we are manipulating an in-memory database or constructing SQL query strings (or whatever else is necessary).
The big win here is that we can use any dataset that can be modelled with the relational algebra.
It is safe to ignore the first type parameter in the `Query` type constructor; the emphasis here is on building query ASTs, not data families.

{% highlight idris %}
-- src/Database/PowerOfPi/Abstract/Query.idr
data Query : (b:Backend) -> (s:Schema) -> Type where
  Table   : TableType b s -> Query b s
  Union   : Query b s -> Query b s -> Query b s 
  Diff    : Query b s -> Query b s -> Query b s 
  Product : Query b s -> Query b s' -> { auto p : Disjoint s s' } -> Query b (s ++ s')
  Projection : (f:String -> Maybe String) -> Query b s -> Query b (projectedSchema f s)
  Select  : Expr s Bool -> Query b s -> Query b s 
  --GroupBy : Eq k => Expr s k -> Query b s -> Query b ["k":::k, "v":::TableType b s ]
  --Lookup  : Eq k => k -> Query b ["k":::k, "v":::TableType b s] 
  [XX remove these for simplicity?]
{% endhighlight %}

Recall that the set operators require additional constraints that less powerful type systems have a difficult time capturing.
Set union and difference requires that the two operands share matching schemas, whereas the Cartesian product requires that they be disjoint.
Notice how easily Idris allows us to express these constraints---particularly for matching schemas.
The Cartesian product constraint is slightly more complex, requiring a decidable proof that the two schemas are disjoint.
[XX this is not implemented]

### Backend Implementations

So far, we are only able to construct (and statically verify) well-typed query trees.
This would be useful if our in-memory representation coincided with some sort of relational algebra standard, but unfortunately that is not the case.
We need to convert our query tree into something a database will understand.

We implemented two backends: a basic in-memory database written with Idris and a SQLite backend.
The in-memory representation is convenient because users have access to the full power of Idris for generating test data and testing queries against it.
The SQLite implementation compiles queries to SQL commands.
[XX is it SQLite specific or do all SQL db's work?]
[XX talk about challenges with external databases]

`Query` abstracts the backends out as a data family.
[XX couldn't we just have used unique identifiers and Data.Maps to implement the same functionality without data families? it feels cleaner, too]
This allows us to represent tables with in-memory representations, as symbolic references to anything else, or just as strings containing their names.
For example, queries built for our Idris backend directly contain the tables they are referring to; tables are implemented as lists of rows (i.e. `List Row`).
The SQLite backend, on the other hand, will compile SQL commands using only the table names.

### TODO:


* discuss projection
* discuss type family used for backend rep.
* discuss Idris DB vs. external DB's; requirements, etc.
