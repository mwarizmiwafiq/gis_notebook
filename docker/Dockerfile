FROM jupyter/tensorflow-notebook:python-3.11.4

# Define environment variable
ENV CPLUS_INCLUDE_PATH=/usr/include/gdal
ENV C_INCLUDE_PATH=/usr/include/gdal

USER root

# Update and install dependencies
RUN apt-get update -y && \
    apt-get install -y \
    build-essential \
    libssl-dev \
    libffi-dev \
    libproj-dev \
    libgdal-dev \
    gdal-bin \
    python3-gdal

RUN apt-get update && apt-get install -y curl gnupg && \
    echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] http://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - && \
    apt-get update && apt-get install -y google-cloud-sdk

# Install other Python packages
RUN pip install rasterio fiona shapely geopandas
RUN pip install earthengine-api
RUN pip install GDAL==$(gdal-config --version | awk -F'[.]' '{print $1"."$2}')
RUN pip install retrying
RUN pip install rio-cogeo
RUN pip install tqdm