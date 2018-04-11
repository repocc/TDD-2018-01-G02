(ns data-processor)

; Idea de la estructura de datos

(
    {"datos" ()}
    {"email-count"
        #user.ReglaCont{:condicion true,
                        :argumentos [],
                        :contadores []}}
    {"spam-count"
        #user.ReglaCont{:condicion (current "spam"),
                        :argumentos [],
                        :contadores []}}
    {"spam-fraction"
        #user.ReglaSign{:dato (/ (counter-value "spam-count" []) (counter-value "email-count" [])),
                        :condicion true}}
    {"spam-important-table"
        #user.ReglaCont{:condicion true,
                        :argumentos [(current "spam") (current "important")],
                        :contadores []}}
)

(def condicionContadores {})
(def condicionSeñales {})
(defrecord ReglaCont [condicion contador])
(defrecord ReglaSign [nombre dato condicion])
 
(defmulti armarFunc (fn [algo] (first algo)))
 
(defn generarNombreContador [nombre args]
  (for [item args]
      (def contadores (assoc contadores (str nombre"-"(last item)) 0))))
 
(defmethod armarFunc (symbol "define-counter") [algo]
        (if (= (count (nth algo 2)) 0)  
          (let [nombre  (nth algo 1)
          condic  (nth algo 3)]
          {nombre (new ReglaCont condic 0)})    
       
        (for [param (nth algo 2)]
            (let [nombre  (nth algo 1)
                  condic  (nth algo 3)]
            (def nombreCompleto (str nombre"-"(second param)))    
            { nombreCompleto (new ReglaCont condic 0)}))))
 
(defmethod armarFunc (symbol "define-signal") [algo]
         (let [dato  (nth algo 1)
                  nombre (first (keys dato))
                  info  ( get dato nombre)
                  condic (nth algo 2)]
            { nombre (new ReglaSign nombre info condic)}))
 
(defn parseList [elemento]
  (if (= (type elemento) clojure.lang.PersistentArrayMap)
      elemento
      (into {} elemento)))
 
(defn initialize-processor [rules]
 (conj (map (fn [diccElement] (parseList diccElement)) (map (fn [item] (armarFunc item)) rules)) {"datos" '()}))
      
(defn process-data [state new-data]
  [nil []])
         
(defn query-counter [state counter-name counter-args]
  0)
