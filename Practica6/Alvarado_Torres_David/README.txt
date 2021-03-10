Nombre:                                 No. de Cuenta:           Correo-e:
David Ignacio Alvarado Torres           316167613                davidalv@ciencias.unam.mx
Adriana Hernández Gasca                 316161570                ady_gasca@ciencias.unam.mx
Laura Itzel Tinoco Miguel               316020189                itzel_tinoco@ciencias.unam.mx

OBSERVACIONES: En la antepenúltima prueba el error que se espera está mal escrito, ya que no coincide con el error lanzado por el quinto test (comenzando desde el último). Podemos ver que el error debe ser el mismo en ambos tests pues en ambos sucede que la condicional del if no es un booleano. Entonces el error que se espera debería ser "if: Type error Conditional's text-expr type must be a boolean Given: (numberT)".  

EXPLICACIÓN PUNTO EXTRA: Para integrar el verificador de tipos a la practica pasada, lo único que hacemos es llamar a la función typeof de la expresión ya 'parseada' antes de intentar interpretarla. En caso de que el verificador no lance un error, ejecutamos desugar sobre la expresión y finalmente la interpretamos. 

Para interpretar una función con nuestro código, primero se debe pasar por el parse. Después podemos ejecutar al intérprete, el cual recibe un único argumento que es la expresión que regresa el parse. No es necesario pasarle el ambiente vacío como argumento a interp.
