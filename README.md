# Data Analyst (Proyecto Académico) – Análisis de Tendencias de Vuelos

## Análisis Exploratorio de Datos con R y dplyr

Me encargaron la tarea de analizar un conjunto de datos de vuelos del segundo semestre de
2022 para extraer tendencias clave y responder a preguntas operativas específicas. Mi objetivo
era transformar los datos en bruto en insights accionables, utilizando R y el paquete dplyr para
la manipulación y agregación de datos.

Mi enfoque se basó en la integración de tres fuentes de datos distintas: un registro detallado de
vuelos, un catálogo de aerolíneas y una base de datos de aeropuertos. Al combinar estas
fuentes, pude enriquecer los datos transaccionales con información descriptiva, como los
nombres completos de las aerolíneas y aeropuertos, lo que fue crucial para presentar
resultados claros y comprensibles.

Para responder a las preguntas de negocio, realicé una serie de análisis enfocados y precisos:

1. Identificación de la Ruta Más Concurrida: Analicé todos los vuelos que partían de los
aeropuertos del área de Nueva York (identificados de manera robusta por su zona
horaria, "America/New_York"). Al agrupar y contar los vuelos, determiné con precisión
la combinación de aerolínea y aeropuerto de destino que representa el corredor aéreo
de mayor volumen, así como la duración promedio de vuelo para esta ruta crítica.

2. Determinación de la Ruta de Mayor Duración: Para identificar las operaciones de largo
alcance, calculé la duración promedio de vuelo en horas para cada ruta que partía de
Nueva York. Esto me permitió identificar el destino que, en promedio, requiere el mayor
tiempo de vuelo, un dato clave para la logística de tripulación y la planificación de rutas.

3. Descubrimiento del Destino Menos Frecuentado: Con un enfoque en el aeropuerto JFK,
realicé un análisis de frecuencia para identificar el aeropuerto de destino que recibe el
menor número de vuelos. Este tipo de insight es valioso para la estrategia de negocio, ya
que puede señalar oportunidades de mercado desatendidas o rutas con baja demanda
que podrían requerir revisión.

Mi trabajo no solo proporcionó respuestas directas a las preguntas planteadas, sino que
también demostró un flujo de trabajo de análisis de datos completo, desde la carga y limpieza
hasta la integración de múltiples tablas y la generación de métricas de negocio.
Logros y Habilidades Demostradas:
+ Extraje insights de negocio clave a partir de datos de vuelos complejos, respondiendo a
preguntas sobre las rutas más y menos frecuentes y las de mayor duración.
+ Demostré una alta competencia en la manipulación de datos con dplyr, utilizando
funciones como filter, group_by, summarise y which.max para realizar análisis
complejos.
+ Integré y gestioné con éxito múltiples fuentes de datos para enriquecer el análisis y
presentar resultados comprensibles (por ejemplo, convirtiendo códigos de
aeropuertos FAA en nombres completos).
+ Realicé análisis de frecuencia y de agregación para cuantificar patrones operativos,
como la identificación del corredor aéreo de mayor tráfico desde Nueva York.
+ Transformé los datos en métricas útiles para el negocio, como la conversión del tiempo
de vuelo de minutos a horas para una mejor interpretación.
• Proporcioné inteligencia de negocio accionable, identificando tanto las rutas de alto
rendimiento como las potenciales oportunidades en destinos menos frecuentados.
