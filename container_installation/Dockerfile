FROM ubuntu:24.04
RUN apt-get update
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get -y install build-essential
RUN apt-get -y install cmake wget gfortran libcurl4-openssl-dev libssl-dev
RUN apt-get -y install software-properties-common gtk-doc-tools libfontconfig1-dev
RUN apt-get -y install libcairo2-dev libgdal-dev libssh2-1-dev libxml2-dev

# HDF5
RUN apt-get -y install libhdf5-serial-dev

# NetCDF4
RUN apt-get -y install libnetcdf-dev netcdf-bin libudunits2-dev

# Add R repository and install R
RUN apt-get -y install dirmngr gnupg apt-transport-https ca-certificates
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/'
RUN apt-get update
RUN apt-get -y install r-base r-base-dev
RUN apt-get -y install libeccodes-dev
RUN apt-get -y install libeccodes-tools
RUN apt-get -y install git
#RUN apt-get -y install libjasper-dev libaec-dev
# Install system dependencies for R packages
RUN apt-get -y install libharfbuzz-dev libfribidi-dev libtiff-dev libfreetype6-dev \
    libpng-dev libjpeg-dev libproj-dev libgeos-dev

# Accept GITHUB_PAT as build argument
ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}

#RUN export ECCODES_DIR=/usr/local
#RUN git clone https://github.com/ecmwf/eccodes.git && \
#    cd eccodes && \
#    mkdir build && \
#    cd build && \
#    cmake .. \
#        -DCMAKE_INSTALL_PREFIX=/usr/local \
#        -DENABLE_NETCDF=ON \
#        -DENABLE_ECCODES_THREADS=ON \
#        -DENABLE_AEC=OFF && \
#    make install
ENV ECCODES_DIR=/usr
RUN codes_info && \
    dpkg -L libeccodes-dev | grep -E "(bin|lib)" && \
    echo "eccodes installation verified"

#RUN R -e "library(remotes); install_github('harphub/Rgrib2')"
# create .Rprofile for all R sessions
# Install R packages
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"
RUN echo 'options(repos = c(CRAN = sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/centos8-%s/%s", R.version["arch"], substr(getRversion(), 1, 3))))' \
    > /root/.Rprofile
RUN ECCODES_DIR=/usr/local \
    R -e "library(remotes); install_github('harphub/Rgrib2')"
RUN R -e "if (!require('Rgrib2')) stop('Rgrib2 not installed')"
RUN R -e "library(remotes); install_github('harphub/harp')"
RUN R -e "library(remotes); install_github('pollyaschm/harpSpatial', 'ACCORD_VS_202406')"
RUN R -e "library(remotes); install_github('harphub/Rfa')"
RUN R -e "install.packages('hdf5r', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('reticulate', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('here', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tidyverse', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dplyr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggpubr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('RColorBrewer', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggplotify', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('patchwork', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ncdf4', repos='https://cloud.r-project.org/')"
