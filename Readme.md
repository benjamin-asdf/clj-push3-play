Another Clojure implementation of the [push3](https://faculty.hampshire.edu/lspector/push3-description.html) programing language. 

Push is a simple, stack based langauge designed as the target domain for program synthesis tasks. 

# Clojush

Clojush, an implementation of Push/PushGP in Clojure that has been used for many research projects since 2010. Clojush is full-featured but has accumulated technical debt and is nontrivial to understand or modify: http://github.com/lspector/Clojush

# clj-push3-play

- Heavily reuse Clojush source code. 
- Be slighly more data oriented.

## Other work

- [propeller](https://github.com/lspector/propeller)

## Interpreter

Preliminary interface: 


``` clojure
(require '[benjamin-schwerdtner.clj-push3-play.interpreter :as push])

(defn execute [program]
  (push/execute
   (push/setup-state)
   program
   {:max-executions 250}))
   
   
(->
 (execute '(2 2 exec_y (integer_dup integer_*)))
 :stacks :push/integer)
 
[2 10000000000]

```



## Examples

- usage example with random code: [gen](examples/gen.clj)
- working on it



# Hyperdimensional Computing (HDC) implementation 

## Why HDC

Introduce a neurosymbolic paradigm,

pros:

- high biological plausibility,
- a computing paradigm that makes sense in the light of neuronal ensembles. 
- proven interesing / promising across cognitive modeling, analogical reasoning applications.
- potential algorithmic layer for emerging neuromorphic hardware.


drawbacks: 

- straightforward symbolic modeling might be simpler for some cases.
- requires a good high performance library, 
- additional prgrammer load for dealing with subsymbolic tensor operations.


## Fourier Holographic Reduced Representation (FHRR)

> Holographic Reduced Representation: Distributed Representation for Cognitive Structures

Tony A. Plate 2003

This model uses complex phaser hypervectors.

file:/src/benjamin_schwerdtner/clj_push3_play/hdc/fhrr.clj

### Why

- Relatively easy to implement a resonator network with.
- supports *fractional power exponantiation* (FPE) algorithm.
- Compares powerfully with other VSA's http://www.arxiv.org/abs/2001.11797.
- drawback 1: Storage need compared to block sparse VSA's. 
- drawback 2: Bind is not self-inverse.

## Resonator Network

> Resonator networks for factoring distributed representations of data structures. 

Frady, Kent, Olshausen, Sommer 2020
Neural Computation 32, 2311-2331 (2020)

https://arxiv.org/abs/2007.03748

file:src/benjamin_schwerdtner/clj_push3_play/hdc/resonator.clj

- WIP: incoorparate improvements from https://arxiv.org/abs/2208.12880v4


### The Problem

Factorize compound data structure representations in HDC/VSA.
Given a hdv resulting from previous binding operations, which are the seed hypervectors contributing to the binding? 

### Resonotor Network Algorithm

The resonator combines computing in superposition and cleanup operations to search the 'factor space' in parallel.
It can be interpreted as a recurrent neuronal net with fixed point dynamics that are the solution to the factorization problem.





### Example Usage

From tests:

``` clojure

(require '[benjamin-schwerdtner.clj-push3-play.hdc.resonator :as resonator])
(require '[benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd])

(require-python '[torch :as torch])

(def reference-impl resonator/exhaustive-search-factorize)

(let [books (torch/stack [(hd/seed 10) (hd/seed 10) (hd/seed 10)])
      [a b c] (mapv (fn [b] (py/get-item b (rand-int 10))) books)
      x (hd/bind [a b c])
      factors-ref (vec (reference-impl x books))
      factors (:factors (resonator/resonator-fhrr x books))]
  (t/is (torch/allclose (torch/stack factors-ref)
                        (torch/stack factors))
        (str "Factorizes")))
```

### Why

- factorization (the flip side of binding) might prove to be a vital module for neurosymbolic systems.
- proven to be an efficient alg by the researchers.

drawbacks: 

- research topic to make work with Binary Sparse Block Codes (BSBC).
- ?? 


## Fractional Power Exponentiation (FPE), Spatial Semantic Pointer (SSP)

> A neural representation of continuous space using fractional binding

Brent Komer, Terrence C. Stewart,
Aaron R. Voelker, and Chris Eliasmith. A neural representation of continuous space using fractional binding. 
In Annual Meeting of the Cognitive Science Society, 2019.

- [FPE impl](src/benjamin_schwerdtner/clj_push3_play/hdc/fractional_power_encoding.clj)
- [SSP impl](src/benjamin_schwerdtner/clj_push3_play/hdc/spatial_semantic_pointer.clj)

## Conceptual Hyperspace (CH)

> Analogical Reasoning Within a Conceptual Hyperspace

Howard Goldowsky, Vasanth Sarathy, 2024
https://arxiv.org/abs/2411.08684

- wip


# Ideas / want to do

- explore algs where the variation operator is itself evolved ['metaevolution'](ideas/metaevolution.org).
  - split 'mutation' and 'world' genomes.
- explore symbiogenic algorithms.
- explore open ended problems and creativity (G. Chaitins Metabiology but applied).
- Implement [HDC/VSA](https://www.hd-computing.com/) operators.
- Figure out a hyper dimensional representation of push code so that the same program can be target of 
  symbolic and neural / subsymbolic algorithms.

# Development



## Run tests

The python path needs to be set, then run `clj -X:test`. 

``` shell
./run.sh  -X:test
``` 
