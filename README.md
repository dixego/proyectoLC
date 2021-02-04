# Proyecto final de Lógica Computacional

Paquete de BDDs desarrollado en Haskell

- `Data/Prop.hs` contiene la representación de fórmulas proposicionales.
- `Data/BDD.hs` contiene la representación de OBDDs reducidos.
- `Data/BDD/Visualization` contiene una función para imprimir los OBDDs en formato de
   graphviz.
- `Main.hs` contiene algunas pruebas del paquete. Se puede ejecutar independientemente
   corriendo `runghc Main.hs` (suponiendo que las bibliotecas necesarias estén disponibles
   en el sistema). Ejecutar este archivo genera varios archivos `.dot` en la carpeta
   actual.
