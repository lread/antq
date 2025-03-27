(ns leiningen.antq
  (:require
   [antq.core]
   [leiningen.core.main :as lein-main]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]))

;; TODO: Validation of antq opts in project.clj
;; TODO: Compare with antq.tool
;; TODO: What about --download and --ignore-locals for lein discovered?

(defn antq
  ;; docstring is also delivered as help for plugin (lein antq --help)
  "Leiningen plugin.

  When checking `./project.clj`, also includes deps that only leiningen sees/evaluates.
  Only deps discovered through static analysis are upgradeable.

  Specify options on the command line as per normal cli syntax, e.g.:
  ```
  lein antq --upgrade
  ```
  You can also specify options in your `project.clj` under `:antq`, e.g.:
  ```
  :antq {:upgrade true}
  ```
  Command line options override project.clj specified options."
  [lein-project & args]
  (let [default-options (cli/get-default-options antq.core/cli-options)
        {cli-options :options cli-errors :errors} (cli/parse-opts args antq.core/cli-options :no-defaults true)
        options (-> default-options
                    (merge (:antq lein-project))
                    (merge cli-options)
                    (update :error-format #(some-> %
                                                   (str/replace #"\\n" "\n")
                                                   (str/replace #"\\t" "\t")))
                    (assoc :lein-context (select-keys lein-project [:dependencies :managed-dependencies :plugins :repositories])))]
    (binding [lein-main/*exit-process?* true]
      (with-redefs [antq.core/system-exit #(lein-main/exit %)]
        (antq.core/main* options cli-errors)))))

