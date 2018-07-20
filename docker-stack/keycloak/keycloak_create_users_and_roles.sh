#!/usr/bin/env bash

set -eu

export PATH=$PATH:/opt/jboss/keycloak/bin

KEYCLOAK_REALM=WebApplication

# Login as admin to master realm
kcadm.sh config credentials --server http://localhost:8080/auth --realm master --user admin --password 'Password1!'
# Create WebApplication realm
kcadm.sh create realms -s realm=$KEYCLOAK_REALM -s enabled=true
# Create WebClient client
kcadm.sh create clients -r $KEYCLOAK_REALM -s clientId=WebClient -s publicClient=true \
    -s rootUrl="http://localhost:8082" -s redirectUris='["http://localhost:8082/*"]' -s enabled=true
# Create ProductReader and CustomerReader roles
kcadm.sh create roles -r $KEYCLOAK_REALM -s name=ProductReader -s description="Product reader"
kcadm.sh create roles -r $KEYCLOAK_REALM -s name=CustomerReader -s description="Customer reader"
# Create vlad and svit users with passwords
kcadm.sh create users -r $KEYCLOAK_REALM -s username=vlad -s enabled=true
kcadm.sh set-password -r $KEYCLOAK_REALM --username vlad --new-password 'vlad'
kcadm.sh create users -r $KEYCLOAK_REALM -s username=svit -s enabled=true
kcadm.sh set-password -r $KEYCLOAK_REALM --username svit --new-password 'svit'
# Assign roles to users (vlad > ProductReader, svit > CustomerReader)
kcadm.sh add-roles -r $KEYCLOAK_REALM --uusername vlad --rolename ProductReader
kcadm.sh add-roles -r $KEYCLOAK_REALM --uusername svit --rolename CustomerReader
