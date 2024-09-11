# Usar una imagen base que tenga R instalado
FROM rocker/r-ver:4.2.2

# Instalar paquetes adicionales
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R necesarios
RUN R -e "install.packages(c('dplyr', 'RODBC', 'lpSolve', 'jsonlite'), repos='https://cloud.r-project.org/')"

# Copiar el script R al contenedor
COPY archivodoker.R /usr/local/bin/archivo.R

# Establecer el directorio de trabajo
WORKDIR /usr/local/bin

# Comando para ejecutar el script
ENTRYPOINT ["Rscript", "archivo.R"]
