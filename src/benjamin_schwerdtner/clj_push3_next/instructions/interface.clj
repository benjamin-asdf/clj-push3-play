(ns benjamin-schwerdtner.clj-push3-next.instructions.interface
  (:require
   [benjamin-schwerdtner.clj-push3-next.instructions.impl :as impl]
   [benjamin-schwerdtner.clj-push3-next.instructions.boolean]
   [benjamin-schwerdtner.clj-push3-next.instructions.code]
   [benjamin-schwerdtner.clj-push3-next.instructions.numbers]
   [benjamin-schwerdtner.clj-push3-next.instructions.push-name]
   [benjamin-schwerdtner.clj-push3-next.instructions.exec]))

(defn register-instruction [{:keys [sym-name in out f] :as opts}]
  (comment sym-name in out f)
  (impl/register-instruction opts))

(defn instruction [sym] (impl/instruction sym))

(defn execute-instruction [state instr]
  (impl/execute-instruction state instr))

(defn all-instructions [] (impl/all-instructions))
