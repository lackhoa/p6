(ns p6.main
  ;; This is node require
  (:require
   ["fs" :as fs]
   ["electron" :refer [app BrowserWindow ipcMain]]))

(def main-window (atom nil))

(defn init-browser []
  ;; Create `main-window` that renders `index.html`
  ;; Path is relative to the compiled js file (resources/main.js)
  (reset! main-window
          (-> {:webPreferences {:nodeIntegration true}}
              (clj->js)
              (BrowserWindow.)))
  (.loadURL @main-window (str "file://" js/__dirname "/public/index.html"))
  (.on @main-window "closed" #(reset! main-window nil)))

(defn main []
  (.on app "window-all-closed" #(when-not (= js/process.platform "darwin")
                                  (.quit app)))
  (.on app "ready" init-browser))
