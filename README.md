![dora](https://raw.githubusercontent.com/fractalLabs/valida-dora/master/resources/validadora.png)
<<<<<<< HEAD

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

## License

Copyright © 2016 MX Abierto

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
=======
Herramienta para correr una serie de procesos y obtener en JSON el resultado de todos.

## Uso

Primero que nada necesitas [lein](http://leiningen.org)

Clona el repo y entra al folder

Y para ejecutar un archivo o directorio haz:

`lein run [ARCHIVO]`

Por ejemplo prueba:

`lein run project.clj`

Las validaciones que ejecuta se encuentran en `src/valida-dora/core.clj` adentro de `(def metas [])`

```clojure
(def metas
  ["head -n 1"
   "file"
   "wc -l"])
```

Cada elemento o validación puede ser una string, que representa algo que corre en el shell y lleva como último argumento el nombre de un archivo.
O una función en Clojure que lleva como argumento un texto (el archivo representado como una string).

## Licencia
[Libre Uso MX](http://datos.gob.mx/libreusomx)
>>>>>>> 55472c386f1329fd1adb3c60d075163213da3865
