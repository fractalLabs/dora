![dora](https://raw.githubusercontent.com/fractalLabs/valida-dora/master/resources/validadora.png)

Herramienta para descargar e inspecionar los datos y metadatos de datos.gob.mx

## Usage

Es necesario instalar [lein](http://leiningen.org)

Y tener una variable de entorno MONGO_URL con la url con los datos de conexión de la base de datos MongoDB.

Clona el proyecto y haz `lein repl`

Para arrancar el web server en el puerto `5555`, haz `(run)

Para popular las bases de datos (o refrescarlas), haz `(data-core)`

Para validar un archivo, haz `(validate "URL")`


Si tienes el servidor arriba, para validar un archivo puedes hacer una petición `GET` a `http://localhost:5555?expr=(validate "URL")`

O para ejecutar otros comandos, en formato de Clojure, usa la api de código de la siguiente forma `http://localhost:5555?expr=EXPRESION`

Para ver la fusión de datos, haz `(fusion "URL")`.

La fusión de datos devuelve un objeto con los siguientes atributos:

`resource` (el objeto de CKAN del recurso)
`dataset` (el objeto de CKAN del dataset al q pertenece)
`catalog` (el contenido del catalogo, de ADELA, de hasta arriba sin lo anidado)
`catalog-dataset` (el contenido del catalogo sobre el dataset, de ADELA, sin la lista de recursos)
`catalog-dataset-resource` (el contenido del catalogo especifico del recurso)
`file-metadata` (la data producto de los scripts de metadatos/validacion)
`recommendations` (la lista de recomendaciones generadas)

Copyright © 2016 MX Abierto

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
