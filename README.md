# Efectos del uso del suelo en especies de vertebrados pampeanos: evaluando escenarios futuros - Ornella Gaspari

Este repositorio contiene el pipeline completo de R utilizado para el proyecto de mi tesis de licenciatura.

### 📁 SCRIPTS/
Contiene todos los archivos `.R` utilizados en el pipeline, organizados y numerados de manera secuencial según su función en el análisis:

*   **`0_libraries.R`**: Carga y gestión de todos los paquetes y librerías de R requeridos para el proyecto.
*   **`1.CroppingIUCNRedList.R`**: Recorte y procesamiento inicial de las capas espaciales de la Lista Roja de la UICN.
*   **`2.CroppingGBIF.R`**: Recorte y pre-procesamiento de los registros de presencia globales descargados de GBIF.
*   **`2.1.SpeciesNamesGBIF.R`**: Extracción y filtrado de los nombres únicos de las especies obtenidos desde GBIF.
*   **`3.1.GetAmphibianData.R`**: Limpieza, conversión de coordenadas (DMS a decimal) y unificación de los datos de campo de anfibios con los datos de GBIF.
*   **`3.2.GetMammalData.R`**: Procesamiento, estandarización e integración de las bases de datos de campo y de GBIF para los mamíferos seleccionados.
*   **`3.3.GetBirdData.R`**: Filtrado de especies de aves preseleccionadas, limpieza de duplicados, corrección de coordenadas e integración de datos.
*   **`4.Filter_occurrence_data_by_year_and_cell.R`**: Filtrado temporal de los registros (período 2010-2020), eliminación de duplicados por celda de raster (resolución SEALS de 300m), depuración manual de puntos fuera de la distribución y generación de la base de datos unificada para los SDM.
*   **`5.customFunctions.R`**: Definición de funciones personalizadas para la clasificación de usos del suelo, asignación de nombres de capas y la ejecución automatizada de los Modelos de Distribución de Especies (SDM).
*   **`6.1.landuse_landscapesPrep.R`**: Reclasificación y simplificación de las categorías de uso del suelo de ESA (39 clases) a las clases de SEALS (7 clases), recorte para Sudamérica y agregación a una resolución objetivo de 1km.
*   **`6.2.GetBioclimData.R`**: Descarga automatizada y recorte de las variables bioclimáticas desde la plataforma CHELSA V2.1.
*   **`6.3.SDM_inputLandscapes.R`**: Preparación final de los paisajes (entrenamiento, actuales y futuros), análisis de correlación (VIF), remoción de la capa de bosque y generación de los stacks finales para los modelos.
*   **`7.SDM_run.R`**: Ejecución en paralelo de los Modelos de Distribución de Especies (algoritmos RF, XGBOOST, ANN, MAXNET) y proyecciones de ensambles para condiciones actuales y escenarios futuros (SSP1, SSP2 y SSP5).
*   **`8.Figures.R`**: Generación de gráficos de idoneidad, cálculo de coberturas porcentuales de uso del suelo y mapas de las proyecciones futuras.
*   **`8.1.VarImportanceEM_table.R`**: Extracción, cálculo de promedios/desviaciones estándar y exportación en formato Word de las tablas de importancia de variables de los modelos de ensamble.
*   **`8.2.Plot_occurrences_and_SDM_Presence_points.R`**: Mapa superponiendo los registros crudos de ocurrencia frente a los puntos de presencia utilizados por los SDM.
*
> **Nota sobre el idioma:** Los comentarios y la documentación interna de los scripts están en inglés, ya que el código fue desarrollado en colaboración con investigadores angloparlantes.

---

### 📁 DATA/
*(Ignorado en GitHub)* Contiene los datos organizados internamente en tres subcarpetas según su etapa de procesamiento: `Raw data`, `Processed data` y `Landscapes` (dividida en `OriginalLandscapes` y `ProcessedLandscapes`).

### 📁 OUTPUT/
*(Ignorado en GitHub)* Almacena los resultados intermedios de los modelos, los archivos ráster continuos/binarios proyectados y todas las figuras y mapas exportados en alta resolución.

---
## Cómo ejecutar el código
Para ejecutar estos scripts de forma local, debes recrear la estructura de las carpetas `DATA/` y `OUTPUT/`. 

Por favor, descarga los conjuntos de datos desde el [enlace](https://drive.google.com/drive/folders/1T2dU5vAKmi48Bdwm-kfzEWg0Lg3AnyKP?usp=drive_link) compartido de Google Drive y colócalos en sus respectivas rutas dentro de la carpeta `DATA/`.

*Nota: Los datos brutos de campo son confidenciales y han sido excluidos de la carpeta de datos compartidos.*
