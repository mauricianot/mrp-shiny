
# INSTALLATION JAVA JDK
## Update package system debian
```
sudo apt-get update
```

## JDK
```
sudo apt install default-jdk
javac -version
sudo update-alternatives --config javac
```

## Configuration profile de l'environement
```
sudo nano /etc/profile
# Ajouter à la dernière ligne de fichier
JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64"
export PATH=$PATH:$JAVA_HOME/bin
```

## Verification profile de l'environement
```
source /etc/profile
echo $JAVA_HOME
```

# OSRM BACKEND MANUEL
## update package system debian
```
sudo apt-get update
sudo apt install build-essential git cmake pkg-config libbz2-dev libstxxl-dev libstxxl1v5 libxml2-dev libzip-dev libboost-all-dev lua5.2 liblua5.2-dev libluabind-dev libtbb-dev jq libosmpbf-dev libprotobuf-dev
```

## Créer compte osrm pour lancer le service
```
sudo useradd -d /srv/osrm -s /bin/bash -m osrm
sudo apt install acl
sudo setfacl -R -m u:username:rwx /srv/osrm/ #(usermane : user account)
```

## Téléchargement d'osrm-backend
```
cd /srv/osrm/
git clone https://github.com/Project-OSRM/osrm-backend.git
cd osrm-backend
git checkout v5.25.0
```

## compiled OSRM
```
mkdir -p build
cd build
cmake ..
cmake --build .
sudo cmake --build . --target install
```

## generate makefile OSRM
```
cmake ..
make
sudo make install
```

## prepare background carte 
```
sudo cp ./mrpOsm.pbf /osrm-backend/mrpOsm.pbf
osrm-extract ./mrpOsm.pbf -p profiles/foot.lua
osrm-contract "mrpOsm.osrm"
```
## créer une service pour l'osrm-route
```
sudo nano /etc/systemd/system/osrm-routed.service
```
## Contenu du fichier de osrm-routed.service
```
[Unit]
Description=Open Source Routing Machine
Wants=network-online.target
After=network.target network-online.target

[Service]
ExecStart=/usr/local/bin/osrm-routed --port 5050 --max-table-size 100000 /srv/osrm/osrm-backend/mrpOsm.osrm
User=osrm
Group=osrm
Restart=always
RestartSec=5s

[Install]
WantedBy=multi-user.target
```
## Autorisation d'écriture d'utlisateur "osrm"
```
sudo chown osrm:osrm /srv/osrm/osrm-backend/ -R
```

## Service osrm-route
```
sudo systemctl start osrm-routed
sudo systemctl enable osrm-routed
systemctl status osrm-routed
```

# DATABASE POSTGRESQL
## update package system debian
```
sudo apt-get update
```

## intsall PostgreSQL & PostGIS
```
sudo apt-get install postgresql postgresql-contrib
sudo apt-get install postgis
```

## crate user database | input your pwd user
```
sudo -u postgres createuser -SDRP shiny 	#pwd: sh1nY@pp
sudo -u postgres createdb -O shiny mrp

# install extension for manipuled data geographic in postgis
sudo -u postgres psql -c "create extension postgis;" mrp
sudo -u postgres psql -c "create extension postgis_raster;" mrp
sudo -u postgres psql -c "create extension postgis_topology;" mrp

# Accès au base de données
sudo su postgres
psql -d mrp
alter table spatial_ref_sys owner to shiny;
alter table layer owner to shiny;
alter table topology owner to shiny;
\q
exit
pg_restore -h localhost -p 5432 -U shiny -d mrp -v "~/data/mrp.sql" #pwd: sh1nY@pp
```

# R, PACKAGE AND SHINY SRVER 
## Install R with Debian
```
sudo apt update
sudo apt install r-base
sudo apt-get install libssl-dev libcurl4-openssl-dev libudunits2-dev libgdal-dev libxml2-dev
```

## Install package R (ggplot2 >= 3.4.0, leafem =0.0.1, osrm = 3.5.0)
```
sudo apt install r-cran-sdmtools r-cran-shiny* r-cran-ggpubr r-cran-rjava r-cran-pander
sudo su - -c "R -e \"install.packages(c('shinyjs','shinyalert', 'shinybusy','shinymanager', 'shiny.i18n','leaflet','leaflet.extras', 'shinydashboard','shinydashboardPlus','shinycssloaders', 'shinyBS','shinyWidgets', 'rgdal', 'sf', 'dplyr', 'rpostgis', 'rjson','jsonlite','ggplot2', 'rmarkdown', 'htmlwidgets', 'DT', 'spdep', 'foreign', 'rgeos','osrm', 'rosm', 'maptools', 'raster', 'maps', 'SpatialPosition','gstat', 'RColorBrewer', 'cartography','lattice', 'sf', 'sp','lubridate','dplyr', 'reshape','geosphere', 'plyr', 'lme4', 'mgcv', 'chron','compare', 'reticulate', 'OpenStreetMap','ggsn', 'ggpubr','ggrepel','hms', 'dipsaus', 'htmltools','purrr', 'bslib','reactable','stringr', 'future', 'ipc', 'promises','mailR', 'rJava', 'tibble','plotly', 'ggthemes', 'survey', 'remotes', 'ggspatial', 'rosm'), repos='https://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages(c('decomp'), repos='http://R-Forge.R-project.org')\""
sudo -i R
install.packages('https://cran.r-project.org/src/contrib/Archive/leafem/leafem_0.0.1.tar.gz', repos=NULL, type='source')
install.packages('https://cran.r-project.org/src/contrib/Archive/osrm/osrm_3.5.0.tar.gz', repos=NULL, type='source')
remotes::install_github("r-dbi/RPostgres")
devtools::install_github("yutannihilation/ggsflabel")
```

## Install Shiny Server
```
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.18.987-amd64.deb
sha256sum shiny-server-1.5.18.987-amd64.deb
sudo apt install gdebi-core
sudo gdebi shiny-server-1.5.18.987-amd64.deb
```

# PROJET "MRP" SUR LE GIT
## Recupérer le projet sur le Git
```
cd /srv/shiny-server/
sudo git clone https://gitlab.com/rmauricianot/mrp.git #username: rmauricianot #pwd: 1994nono
sudo chown shiny:shiny /srv/shiny-server/mrp -R
sudo systemctl restart shiny-server
```

## Install package pdflatex (rapoort)
```
sudo apt-get install texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra
```

## Java config R
```
sudo R CMD javareconf
sudo nano /usr/lib/R/etc/javaconf
sudo R CMD javareconf /usr/lib/jvm/java-11-openjdk-amd64/include/jni.h
```