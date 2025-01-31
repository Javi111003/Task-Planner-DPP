# Título del Proyecto

## Planificador de Tareas y Recursos en un Centro de Trabajo

# Descripción del Proyecto:

El proyecto consiste en desarrollar un sistema en Haskell que genere un cronograma óptimo para la asignación de tareas en un centro de trabajo. Este sistema tomará en cuenta las disponibilidades de los trabajadores, las habilidades requeridas por las tareas, la disponibilidad de recursos o herramientas, y otras restricciones (como priorización de tareas o dependencias entre ellas).
Los objetivos principales del planificador son:

1. Garantizar que cada tarea sea realizada por un trabajador adecuado y en un horario válido.
2. Respetar la disponibilidad de los recursos necesarios para completar las tareas.
3. Crear un cronograma eficiente que minimice tiempos muertos y balancee la carga laboral entre los trabajadores.

# Justificación del uso de programación declarativa:

El proyecto utilizará Haskell:
 • Modelar relaciones complejas: Los horarios, recursos y tareas se representan directamente como hechos.
 • Simplificar la solución: Haskell explora automáticamente todas las combinaciones posibles hasta encontrar las asignaciones válidas, sin necesidad de escribir algoritmos imperativos complejos.
Características destacadas:
 • Priorización automática de tareas según su importancia o dependencias.
 • Generación de cronogramas respetando horarios de disponibilidad, habilidades, y recursos.
 • Equilibrio en la carga de trabajo entre empleados.
 • Sistema extensible para agregar nuevas restricciones o mejorar las reglas de optimización.
