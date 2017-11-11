Orden de los scripts para el estudio del papel de la estructura en la riqueza y abundancia de aves nemorales

- Classificacio_ocells… son los que crean las variables de riqueza y abundancia para cada categoria de aves definida.

- ScientificModels: estan definidos los modelos a testar
- Parameter_Vectors: los vectores de parámetros para esos modelos
- zz_AMZcolors: funcion para obtener los colores de las figuras
- zz_mf_labeller: para etiquetar las variables sin necesidad

AJUSTE DE LOS MODELOS
- ModelFitting_Pool: ajuste del mejor modelo para cada especie y grupo, sin separar por tipo de bosque
- ModelFitting_Site: ajuste del mejor modelo para cada especie y grupo, separando entre parcelas de uncinata y silvestris
- ImportModelFits: una vez definidos los modelos, crea un fichero con los AICc, R2, etc de cada modelo, y otro con los parámetros
- SelectBestFits: selecciona, para cada variable respuesta (especie o grupo), el mejor modelo, es decir, aquel con menor AICc
- PlotFigures: crea las figuras. Hay que definir el vector de especies o grupos que queremos representar juntos, después, esta todo automatizado y los representa en función de todas las variables explicativas.

