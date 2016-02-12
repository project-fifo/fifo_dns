#!/usr/bin/bash

USER=fifo_dns
GROUP=$USER
AWK=/usr/bin/awk
SED=/usr/bin/sed

case $2 in
    PRE-INSTALL)
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating fifo_dns group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating fifo_dns user ...
            useradd -g $GROUP -d /var/db/fifo_dns -s /bin/false $USER
            echo "Granting permissions to use low port numbers"
            /usr/sbin/usermod -K defaultpriv=basic,net_privaddr $USER
        fi
        echo Creating directories ...
        mkdir -p /var/db/fifo_dns/ring
        chown -R fifo_dns:fifo_dns /var/db/fifo_dns
        mkdir -p /var/log/fifo_dns/sasl
        chown -R fifo_dns:fifo_dns /var/log/fifo_dns

        ;;
    POST-INSTALL)
        svccfg import /opt/local/fifo-fifo_dns/share/fifo_dns.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`
        CONFFILE=/opt/local/fifo-fifo_dns/etc/fifo_dns.conf
        ZONEFILE=/opt/local/fifo-fifo_dns/etc/fifo.zone.json

        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
            $SED -i bak -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
        else
            echo "Please make sure you update your config according to the update manual!"
            #/opt/local/fifo-sniffle/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
            #    mv ${CONFFILE} ${CONFFILE}.old &&
            #    mv ${CONFFILE}.new ${CONFFILE}
        fi
        if [ ! -f "${ZONEFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${ZONEFILE}.example ${ZONEFILE}
            $SED -i bak -e "s/127.0.0.1/${IP}/g" ${ZONEFILE}
        else
            echo "Please make sure you zonefile is up to date!"
        fi
        ;;
esac
