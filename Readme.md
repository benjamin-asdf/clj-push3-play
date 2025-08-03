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

# Ideas / want to do

- explore algs where the variation operator is itself evolved ['metaevolution'](ideas/metaevolution.org).
  - split 'mutation' and 'world' genomes.
- explore symbiogenic algorithms.
- explore open ended problems and creativity (G. Chaitins Metabiology but applied).
- Implement [HDC/VSA](https://www.hd-computing.com/) operators.
- Figure out a hyper dimensional representation of push code so that the same program can be target of 
  symbolic and neural / subsymbolic algorithms.
  
  
  
