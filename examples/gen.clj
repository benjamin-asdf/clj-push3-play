(ns gen
  (:require
    [benjamin-schwerdtner.clj-push3-play.generator :as gen]
    [benjamin-schwerdtner.clj-push3-play.instructions.interface :as
     instructions]
    [benjamin-schwerdtner.clj-push3-play.interpreter :as push]))

(defn execute [thecode]
  (-> (push/execute (push/setup-state) thecode {:max-executions 1e4}) :stacks))



















(comment

  (defn code []
    (into (list)
          (->
           (push/execute
            (->
             (push/setup-state)
             (assoc-in [:parameters :max-points-in-random-expressions] 250))
            (list 50 'code_rand))
           :stacks
           :push/code)))

  (time (doall (for [n (range 1000)]
                 (let [thecode (code)]
                   (def thecode thecode)
                   (-> (push/execute (push/setup-state)
                                     thecode
                                     {:max-executions 1e4})
                       :stacks)))))






  ;; long overflow:
  ;; now a big number
  (def long-overflow
    '((((76 86 #{}))
       (push_gensym_156983 28)
       true
       exec_do*range
       integer_*
       ()
       nil
       push_gensym_156982
       push_gensym_156981
       push_gensym_156980
       integer_dup
       push_gensym_156979)))

  (execute long-overflow)
  ;; 169095539031224786783005943054018809504857809832788542811264853404115852114259629956294246400000000000000N







  )
