## Code Style Guidelines
- **Imports**: Use `:require` with ns aliases (e.g., `[clojure.string :as string]`)
- **Naming**: Use kebab-case for vars/functions; end predicates with `?` (e.g., `is-top-level-form?`)
- **Error handling**: Use `try/catch` with specific exception handling; atom for tracking errors
- **Formatting**: 2-space indentation; maintain whitespace in edited forms
- **Namespaces**: Align with directory structure (`clojure-mcp.repl-tools`)
- **Testing**: Use `deftest` with descriptive names; `testing` for subsections; `is` for assertions
- **REPL Development**: Prefer REPL-driven development for rapid iteration and feedback
- Don't use the clojure -X:lint tool in the workflow
