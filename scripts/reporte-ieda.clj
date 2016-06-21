(use 'clojure.set)
(def ga (map #(select-keys % [:url :value]) (db :google_analytics)))
(def ieda (db :ieda))

(def eljoin (join  ga  ieda {:url :url}))
(def urlsga (map :url ga))

(defn not-there [url] (empty? (filter #(= % url) urlsga)))

(def eldisjoin (filter #(not-there (:url %)) ieda))
(def todos (concat eljoin eldisjoin))

;(csv "reporte-ieda-trafico.csv" todos)

(def docopi (re-seq #"[^\n]+" "
http://www.siam.economia.gob.mx/work/models/siam/Resource/Avisos/opendata.csv
http://datos.sedesol.gob.mx/padronbeneficiarios/140930/sp/DIRECTORIO_OSC.csv
http://www.conapo.gob.mx/work/models/CONAPO/Proyecciones/Datos/Bases_de_Datos/Proyecciones_Municipios/CSV/baseprymunMX.csv
http://fs.planeacion.sep.gob.mx/cct/cct.zip
http://www.conapo.gob.mx/work/models/CONAPO/Proyecciones/Datos/Bases_de_Datos/Proyecciones_Nacional_y_Entidades/CSV/basepryentMX.csv
http://www.conapo.gob.mx/work/models/CONAPO/Proyecciones/Datos/Bases_de_Datos/Proyecciones_Localidades/CSV/baseprylocMX.csv
http://fs.planeacion.sep.gob.mx/cct/cct09.csv
http://fs.planeacion.sep.gob.mx/cct/cct30.csv
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Mexico.kml
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Oaxaca.kml
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Chiapas.kml
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Puebla.kml
http://web.coneval.gob.mx/Informes/Pobreza/Datos_abiertos/Indicadores_municipales_sabana_DA.csv
http://fs.planeacion.sep.gob.mx/cct/cct21.csv
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Jalisco.kml
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Michoacan.kml
http://fs.planeacion.sep.gob.mx/cct/cct15.csv
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Veracruz.kml
https://www.prospera.gob.mx/Portal/php/padrones/pad_fams_prospera_/15_S072_2014_5.rar
http://www.conapo.gob.mx/work/models/CONAPO/Proyecciones/Datos/Bases_de_Datos/Indicadores_demograficos/CSV/baseindidemMX.csv
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Coahuila.kml
http://datos.sedesol.gob.mx/padronbeneficiarios/140930/sp/CLUNI_ACTIVIDADES_OSC.csv
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Aguascalientes.kml
http://sig2.ran.gob.mx/datosAbiertos/paqueteDatos/RAN_NucleosAgrarios_Chihuahua.kml
http://web.coneval.gob.mx/Informes/Pobreza/Datos_abiertos/Rezago-Social-2010-localidades_DA.csv
"))

(defn is-opi? [url]
  (not (empty? (filter #(= url %) docopi))))

(def full-data (map #(assoc % :opi (is-opi? (:url %))) todos))

(csv "reporte-cotejado.csv" full-data)
