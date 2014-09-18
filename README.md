proof
====

**Proof** is a markdown language for writing structured proofs which compiles to an HTML page. It is heavily inspired by the ideas of [Leslie Lamport](http://research.microsoft.com/en-us/um/people/lamport/pubs/lamport-how-to-write.pdf) and by type-theory based proof assistants (specifically [Coq](http://coq.inria.fr/) and [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php)). The aim and the promise of structured proofs is to make mathematics

- more easily checked for correctness.
- easier to read and understand.

The remainder of the document explains how to use the **Proof** language. Before proceeding, please check out an example of a compiled proof file [here](todo).

### File Structure

A proof file consists of a sequence of declarations. You can declare definitions, theorems, and top level prose blocks. For example,

```
definition [| irrational |] [
  [| A number $x \in \mathbb{R}$ is said to be irrational
     if $x \notin \mathbb{Q}$.
  |]
]
```
or
```
theorem [| $\sqrt{2}$ is irrational |] 
  suppose [|$\sqrt{2} \in \mbb{Q}$|]
  then [|Contradiction|]
  [ %Details of proof go here.
  ]

```
Top level prose blocks are called `comment`s and look like this:
```
comment [| Historical note |] [|
  It has long been known that $\sqrt{2}$ is irrational.
  It is interesting to note that Hippasus of Metapontum
  is supposed to have been sentenced to death by drowning
  for proving this fact in the 5th century BC.
|]
```
Let's see exactly how to write each of these things.

## Latex blocks

Latex is the type setting workhorse of the modern mathematician, and use of Latex
is pervasive in **proof**. Latex blocks are written as follows
```
[| % Latex goes here.
|]
```
Just like Latex, **proof** uses `%` for line comments.

## Definitions

Definitions take the form
```
definition LATEXBLOCK LATEXBLOCK
```
The first block gives the high level name of the definition which is displayed
when the definition node is collapsed. The second block gives the text of the
definition.
TODO: screenshot

## Theorems and lemmas
TODO: explain labels

Top level theorems are written as follows
```
theorem LATEXBLOCK PROPOSITION PROOF
```
The Latex block is, again, the high level name of the theorem which is displayed when the
theorem node is collapsed. The proposition is the statement of the theorem and the proof is,
of course, the proof of the theorem. The syntactic structure of propositions and proofs is
explained in the following sections.

One can also declare facts as lemmas using the keyword `lemma` in place of `theorem`.

### Propositions

Propositions take one of the following forms

- ```
suppose [
    % Comma separated list of assumptions
] then [
    % Comma separated list of results
]
```
- A Latex block
- ```
exists  [
    % Comma separated list of latex blocks
] such that [
    % Comma separated list of Latex blocks
]
```

### Proofs

A proof is either

- A Latex Block 
 - E.g., `[| Since $A \implies B$ and $A$ holds, $B$ holds. |]`
- A comma separated list of *steps*, which are explained in the following section

A proof is structured as a tree whose leaves are Latex blocks and whose
internal nodes are essentially smaller claims on the way to the theorem.

### Steps

Steps are the building blocks of proofs. A *step* is either

- A `claim`
 - Claims have the form
```
claim LATEXBLOCK PROOF
```
and are the usual small statements one makes in the proof of a theorem. For example,
if we were proving the irrationality of `$\sqrt{2}$`, we might write
```
claim [| It suffices to show that assuming $\sqrt{2}$ leads to a contradiction |] [|
       Definition of irrational.
|]
```
- A `cases` block
 - A case block has the form
```
cases [
]
```
