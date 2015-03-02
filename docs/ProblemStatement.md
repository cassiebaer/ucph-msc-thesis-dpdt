# Problem Statement

With each passing day, more and more of our personal information is being collected, cataloged and analyzed by an ever increasing number of interested parties.
Their interests range from simple targeted advertising to malicious and sometimes illegal actions and everything in between.
We are the producers of this data, however, and we should be concerned with how it is being used.

Our medical records consists of (for most of us) very personal information which we reasonably expect to remain private.
Many countries have imposed regulations requiring a baseline of privacy for systems maintaining such sensitive information, e.g, the Health Insurance Portability and Acountability Act (HIPAA) in the USA.
However, the aggregation and availability of an entire populations medical records would be a huge boon for medical researchers in need of statistical data.
And so we are faced with a classic balancing act: how do we balance individuals’ privacy against the usefulness of a dataset?

Differential privacy is an emerging field aiming to answer this question.
The central concept in differential privacy is indistinguishability, i.e, a query against a dataset should return more-or-less the same result regardless of whether or not a particular individual’s records were included in the data.
If the results are indistinguishable, then the records of that particular individual must be unidentifiable.

Many metrics and algorithms have been developed by the differential privacy community.
Each algorithm is typically bundled with a formal proof that it meets some demands or has some privacy-related properties.
The burden of producing such a proof is typically manual, and therefore error-prone.
We aim to solve this problem from a programming languages perspective by allowing the computer to take over this burden.

In some branches of mathematics we concern ourselves with types of things and the operations available to manipulate them (e.g, consider the set of all real numbers and common arithmetic functions).
Programming languages take advantage of this concept by statically verifying programs for type-correctness, precluding many potential sources of runtime errors: “well-typed programs can’t go wrong”.

We aim to extend this notion of being “well-typed” to include differential privacy metrics; i.e, well-typed programs in our language can’t go wrong and also can’t return results which could be used to identify a particular individual included in the data set.
Formal proofs for type-correct algorithms written in our language are unnecessary - the program is the proof! Thus, all type-correct programs must respect expected privacy requirements.

## Learning Objectives

At the end of the semester, the students should be able to:

* implement a prototype of a programming language with differential privacy built into the type-system
* explain essential concepts from differential privacy, e.g. metrics and algorithms
* explain the basics of dependent types and motivate their usage
* analyze the use of dependent types for enforcing differential privacy constraints
* design a simple, embedded domain-specific (type) language (EDSL)
* demonstrate examples of differential privacy algorithms in the prototype language
* justify design decisions made during the implementation of the prototype language (from a programming languages perspective)

## Rough Implementation Outline

We plan to embed our implementation within the dependently-typed language, Idris (idris-lang.org), as an EDSL.
This will allow us to take advantage of Idris' parser and advanced type-system as we explore the embedding of differential privacy metrics.
