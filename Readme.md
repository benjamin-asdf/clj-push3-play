Another Clojure implementation of the [push3](https://faculty.hampshire.edu/lspector/push3-description.html) programing language. 

Push is a simple, stack based langauge designed as the target domain for program synthesis tasks. 

# Clojush

Clojush, an implementation of Push/PushGP in Clojure that has been used for many research projects since 2010. Clojush is full-featured but has accumulated technical debt and is nontrivial to understand or modify: http://github.com/lspector/Clojush

# clj-push3-play

- Heavily reuse Clojush source code. 
- Be slighly more data oriented.


## Interpreter

Preliminary interface: 


``` clojure
(require '[benjamin-schwerdtner.clj-push3-play.interpreter :as push])

(defn execute [program]
  (push/execute
   (push/setup-state)
   program
   {:max-executions 250}))
```

## Examples

- working on it



# Ideas / want to do

- explore algs where the variation operator is itself evolved ['metaevolution'](ideas/metaevolution.org).
- explore symbiogenic algorithms.
- explore open ended problems 


 







