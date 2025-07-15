(ns busy-beaver
  (:require
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as
    instructions]
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.mutation.mut :as mut]))
