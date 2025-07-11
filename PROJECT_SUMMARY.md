# clj-push3-play Project Summary

## Overview

clj-push3-play is a toy Clojure implementation of the Push3 programming language. Push is a stack-based language designed specifically for program synthesis tasks. This implementation aims to provide a cleaner, more data-oriented interface compared to the existing Clojush implementation, separating the Push3 interpreter from genetic programming frameworks.

## Project Philosophy

- **Data-Oriented Design**: Unlike Clojush which uses macros extensively, this implementation provides a data-oriented interface to Push
- **Separation of Concerns**: The Push3 interpreter is decoupled from genetic programming frameworks
- **Clean Architecture**: Aims to avoid the technical debt accumulated in Clojush while maintaining feature parity

## Key File Paths

### Source Code
- `/src/benjamin_schwerdtner/clj-push3-play/interpreter.clj` - Main interpreter implementation (currently in initial development)

### Documentation
- `/Readme.md` - Basic project introduction and comparison with Clojush
- `/BIG_IDEAS.md` - Theoretical foundations exploring program synthesis, elegance detection, and cognitive development
- `/LLM_CODE_STYLE.md` - Comprehensive coding style guide for LLM-assisted development
- `/CLAUDE.md` - Additional code style guidelines specific to this project

### Configuration
- `/deps.edn` - Clojure dependencies configuration (currently empty - no external dependencies)

## Dependencies

Currently, the project has no external dependencies and runs on:
- Clojure Version: 1.12.0
- Java Version: 24.0.1

## Architecture & Components

### Current State
The project is in early development with the core interpreter namespace established but not yet implemented. The architecture will focus on:

1. **Stack-Based Execution Model**: Push3 operates on multiple typed stacks
2. **Data-Oriented Instructions**: Instructions as data rather than macros
3. **Modular Design**: Clear separation between interpreter core and extensions

### Planned Components
- Push3 interpreter core
- Stack management system
- Instruction set definitions
- Program execution engine
- Type system for Push3 types

## Implementation Patterns & Conventions

### Clojure Style Guidelines (from LLM_CODE_STYLE.md)
- **Conditionals**: Use `if` for single conditions, `cond` only for multiple branches
- **Variable Binding**: Minimize `let` bindings, inline single-use values
- **Destructuring**: Use in function parameters for multiple key access
- **Control Flow**: Prefer early returns with `when`, minimize nesting
- **Function Design**: Small, pure functions that do one thing
- **Library Usage**: Prefer `clojure.string` over Java interop
- **Testing**: Always reload namespaces with `:reload` before testing

### REPL Development
- Primary development approach using REPL-driven workflow
- Always reload namespaces: `(require '[namespace] :reload)`
- Switch to working namespace: `(in-ns 'namespace)`

## Development Workflow

1. **REPL-First Development**: Use the REPL for rapid iteration and testing
2. **Data-Oriented Testing**: Test Push programs as data structures
3. **Incremental Building**: Build up the interpreter incrementally, testing each component
4. **Reference Implementation**: Use Clojush as a reference while avoiding its architectural issues

## Extension Points

### For Future Development
1. **Instruction Set Extension**: Add new Push3 instructions as data definitions
2. **Type System Enhancement**: Extend the type system for domain-specific needs
3. **Performance Optimization**: Profile and optimize hot paths in the interpreter
4. **Integration Points**: Create clean APIs for integration with:
   - Genetic programming frameworks
   - Program synthesis systems
   - Educational tools
   - Research experiments

### Potential Features
- Program visualization tools
- Debugging support for Push programs
- Performance profiling
- Alternative execution strategies (parallel, lazy, etc.)

## Usage Examples (Planned)

```clojure
;; Example of running a Push program (planned API)
(require '[benjamin-schwerdtner.clj-push3-play.interpreter :as push])

;; Define a simple Push program as data
(def program '[1 2 INTEGER.+])

;; Execute the program
(push/execute program)
;; => {:integer '(3), :exec '(), ...}

;; Define custom instructions
(def my-instruction
  {:name 'MY.CUSTOM
   :function (fn [state] ...)
   :types [:integer :float]})
```

## Related Resources

- [Push3 Description](https://faculty.hampshire.edu/lspector/push3-description.html) - Official Push3 language specification
- [Clojush Repository](http://github.com/lspector/Clojush) - Reference implementation
- Program synthesis research papers and resources

## Notes for LLM Assistants

When working on this codebase:
1. Follow the style guidelines in LLM_CODE_STYLE.md strictly
2. Use REPL-driven development for testing changes
3. Keep functions small and focused
4. Prefer data structures over macros
5. Always reload namespaces before testing
6. Reference Clojush for feature ideas but not architectural patterns
