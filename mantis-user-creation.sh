#!/bin/bash

#####################################################################################################
# Script para generar usuarios de alumnes en Mantis						    #
# Para usarlo hay que:										    #
# 1- copiar la lista de mails de una planilla (la de asistencias por ejemplo)			    #
# 2- guardarla en un archivo de nombre mantis-user-creation-emails.txt al mismo nivel que el script #
# 3- darle permisos de ejecucion, con chmod +x <nombre_del_script>				    #
# 4- ejecutarlo, con ./<nombre_del_script>							    #
# 												    #
# Los usuarios se crean con los siguientes datos:						    #
# - mail: mail											    #
# - username: username del mail (lo que esta antes del arroba)					    #
# - password: "cambiame"									    #
# - nombre: username										    #
#####################################################################################################

generate_cookie_string() {
    # el manual de uuidgen dice que el uuid generado puede ser considerado unico
    echo $(uuidgen)$(uuidgen) | sed -e 's/-//g'
}

generate_username_from_email() {
    echo $1 | cut -d'@' -f1
}

generate_password() {
    echo 'cambiame' | md5sum | cut -d' ' -f1
}

create_user_in_db() {
    local DB_HOST='1.1.1.1'
    local DB_USER='nahual'
    local DB_PASS=''
    local DB_NAME='mantis_nahual'

    local USER=$(generate_username_from_email $1)
    local PASS=$(generate_password)
    local EMAIL=$1
    local ENABLED=1
    local ACCESS_LVL=25
    local COOKIE=$(generate_cookie_string)
    local REALNAME=$USER # podriamos tomarlo de otra columa del excel de alumnes

    local QUERY="INSERT INTO mantis_user_table ( username, email, password, enabled, access_level, cookie_string, realname ) VALUES ('"$USER"', '"$EMAIL"', '"$PASS"', "$ENABLED", "$ACCESS_LVL", '"$COOKIE"', '"$REALNAME"')"
    #mysql --execute=$QUERY --host=$DB_HOST --database=$DB_NAME --user=$DB_USER --password=$DB_PASS
    echo "Ejecutando >>> $QUERY"
}


if [ $1 -a -e $1 ]; then
    emails_file=$1
    for email in $(cat $emails_file) ; do
	create_user_in_db $email
    done    
else
    echo "Buuuuuuuuuu..."
    echo "No me pasaste un archivo del cual sacar los mails de los users a crear."
    echo "Copialos de una lista (por ejemplo la de asistencias) en un archivo"
    echo "y ejecuta el script con la ruta a ese archivo como argumento"
    echo "./<script> <archivo_de_mails>"
fi
