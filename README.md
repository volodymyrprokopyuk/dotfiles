# Installation

Install fonts:
```bash
cp SourceCodePro ~/.fonts
fc-cache -f -v
fc-list | grep SourceCodePro
```

Install build tools:

```bash
sudo apt-get install build-essential texinfo
```

Install utilities:

```bash
sudo apt-get install unrar xsel
```

Install tools:

```bash
sudo apt-get install git silversearcher-ag htop xmlstarlet jq apg pwgen
```

Install Ansible:

```bash
sudo apt-get install openssh-server
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

sudo add-apt-repository -y ppa:ansible/ansible
sudo apt-get update
sudo apt-get install ansible
```

Install Emacs:

```bash
sudo add-apt-repository -y ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot
```

Install Zsh:

```bash
sudo apt-get install zsh
chsh -s $(which zsh)
```

Install Tmux:

```bash
sudo apt-get install tmux
```

Install dotfiles:

```bash
git clone git@github.com:volodymyrprokopyuk/dotfiles.git ~/.dotfiles
```

Install Java:

```bash
sudo apt-get install openjdk-8-jre openjdk-8-jdk
```

Install Docker:
```bash
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu xenial stable"
sudo apt-get update
sudo apt-get install docker-ce
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker
```

Install PostgreSQL (done):
```bash
# host: localhost, port: 5432, user: postgres, password: postgres, database: postgres
docker run -d --name postgres \
    -e POSTGRES_PASSWORD=postgres \
    -p 5432:5432 \
    postgres

docker exec -it postgres bash
su - postgres
psql
CREATE DATABASE people;
CREATE USER family WITH ENCRYPTED PASSWORD 'family';
GRANT ALL PRIVILEGES ON DATABASE people TO family;

# host: localhost, port: 5432, user: family, password: family, database: people
psql -U family -d people
CREATE SCHEMA family;
CREATE TABLE IF NOT EXISTS family.person(id SERIAL NOT NULL, first_name TEXT NOT NULL, last_name TEXT NOT NULL, PRIMARY KEY (id));
INSERT INTO family.person(first_name, last_name) VALUES ('Volodymyr', 'Prokopyuk');
SELECT * FROM family.person;
```

Install SQL Server:
```bash
# host: localhost, port: 1433, user: sa, password Password!1, database: master
docker run -d --name sqlserver \
    -e ACCEPT_EULA=Y \
    -e MSSQL_PID=Developer \
    -e SA_PASSWORD='Password!1' \
    -p 1433:1433 \
    microsoft/mssql-server-linux

docker exec -it sqlserver /opt/mssql-tools/bin/sqlcmd -S localhost -U sa -P 'Password!1' -d master
CREATE DATABASE people;
go
USE people;
go
CREATE LOGIN family WITH PASSWORD = 'Password!1';
go
CREATE USER family FOR LOGIN family;
go
EXEC sp_addrolemember 'db_owner', 'family';
go

# host: localhost, port: 1433, user: family, password: Password!1, database: people
docker exec -it sqlserver /opt/mssql-tools/bin/sqlcmd -S localhost -U family -P 'Password!1' -d people
CREATE SCHEMA family;
go
CREATE TABLE family.person(id INT NOT NULL IDENTITY, first_name NVARCHAR(100) NOT NULL, last_name NVARCHAR(100) NOT NULL, PRIMARY KEY (id));
go
INSERT INTO family.person(first_name, last_name) VALUES ('Volodymyr', 'Prokopyuk');
go
SELECT * FROM family.person;
go
```

## SQL Server Administration

### Login (Server instance)
```sql
SELECT * FROM sys.server_principals
```

Login > User
```sql
SELECT sp.name login, dp.name [user]
FROM sys.server_principals sp JOIN sys.database_principals dp ON dp.sid = sp.sid
```

### User (Database principal)
```sql
SELECT * FROM sys.database_principals
```

User > Schema
```sql
SELECT dp.name [user], s.name [schema]
FROM sys.database_principals dp JOIN sys.schemas s ON s.principal_id = dp.principal_id
```

User > Role
```sql
SELECT dpu.name [user], dpr.name [role]
FROM sys.database_role_members drm
    JOIN sys.database_principals dpu ON dpu.principal_id = drm.member_principal_id
    JOIN sys.database_principals dpr ON dpr.principal_id = drm.role_principal_id
```

User > Permissions
```sql
SELECT dp.class_desc [object], dp.permission_name [permission], dp.state_desc [grant]
FROM sys.database_permissions dp
WHERE dp.grantee_principal_id = (SELECT principal_id FROM sys.database_principals WHERE name = '<USER>')
```

### Role (Security permissions)
```sql
SELECT * FROM sys.database_principals dp WHERE dp.[type] = 'R'
```

### Schema (Database obejcts)
```sql
SELECT * FROM sys.schemas
```

### Effective permissions
```sql
SELECT * FROM fn_my_permissions(NULL, 'SERVER')
SELECT * FROM fn_my_permissions(NULL, 'DATABASE')
SELECT * FROM fn_my_permissions('family', 'LOGIN')
SELECT * FROM fn_my_permissions('family', 'USER')
SELECT * FROM fn_my_permissions('db_owner', 'ROLE')
SELECT * FROM fn_my_permissions('family', 'SCHEMA')
SELECT * FROM fn_my_permissions('family.fullName', 'OBJECT')
```

Install RabbitMQ:
```bash
# host: localhost, port: 5672, management port: 15672, user: guest, password: guest
docker run -d --name rabbitmq \
    --hostname rabbitmq \
    -p 5672:5672 \
    -p 15672:15672 \
    rabbitmq:3-management

docker exec -it rabbitmq bash
rabbitmqctl list_users
rabbitmqadmin list exchanges
```

Install Keycloak (PostgreSQL):
```bash
# host: localhost, port: 5432, user: keycloak, password: keycloak, database: keycloak
docker exec -it postgres bash
su - postgres
psql
CREATE DATABASE keycloak;
CREATE USER keycloak WITH ENCRYPTED PASSWORD 'keycloak';
GRANT ALL PRIVILEGES ON DATABASE keycloak TO keycloak;

# host: localhost, port: 9090, user: admin, password: admin
docker run -d --name keycloak \
    -e KEYCLOAK_LOGLEVEL=DEBUG \
    -e KEYCLOAK_USER=admin \
    -e KEYCLOAK_PASSWORD=admin \
    -e POSTGRES_ADDR=127.0.0.1 \
    -e POSTGRES_PORT=5432 \
    -e POSTGRES_DATABASE=keycloak \
    -e POSTGRES_USER=keycloak \
    -e POSTGRES_PASSWORD=keycloak \
    --net=host \
    jboss/keycloak -Djboss.socket.binding.port-offset=1010
docker logs -f keycloak
```

Install MongoDB:
```bash
# host: localhost, port: 27017
docker run -d --name mongodb mongo
```

Install Elasticsearch:
```bash
# host: localhost, REST port: 9200, transport port: 9300
docker run -d --name elasticsearch \
    -e "discovery.type=single-node" \
    -p 9200:9200 -p 9300:9300 \
    docker.elastic.co/elasticsearch/elasticsearch:6.2.3
```

Install Elasticsearch for Graylog:
```bash
# host: localhost, REST port: 9200, transport port: 9300
docker run -d --name glelasticsearch \
    -e "discovery.type=single-node" \
    -e "cluster.name=graylog" \
    -p 127.0.0.1:9200:9200 -p 127.0.0.1:9300:9300 \
    docker.elastic.co/elasticsearch/elasticsearch:6.2.3

sudo sysctl -w vm.max_map_count=262144

docker run -d --name glelasticsearch \
    -e "http.host=0.0.0.0" \
    -e "xpack.security.enabled=false" \
    docker.elastic.co/elasticsearch/elasticsearch:6.2.3
```

Install Graylog (MongoDB, Elasticsearch):
```bash
# host: localhost, port: 9100, user: admin, password: admin
docker run -d --name graylog \
    -e GRAYLOG_PASSWORD_SECRET=GraylogPasswordSecret \
    -e GRAYLOG_ROOT_PASSWORD_SHA2="$(echo admin | shasum -a 256)" \
    -e GRAYLOG_WEB_ENDPOINT_URI="http://127.0.0.1:9000/api" \
    --link mongo:mongo --link glelasticsearch:elasticsearch \
    -p 9000:9000 -p 12201:12201 -p 514:514 \
    graylog2/server
```

Install Node:

```bash
nvm ls-remote
nvm ls
nvm install <version>
nvm alias default <version>
```

Install Node utilities:

```bash
npm install -g js-beautify eslint
```

# Update & Configuration

```bash
cd ~/.dotfiles
./play
```

Install and use locally generated SSH key on a remote server:
```bash
# generate SSH key pair locally. Provide SSH key file location ($HOME/.ssh/id_rsa_<provider>) and passphrase
# file parmissions: ~/.ssh = 700, ~/.ssh/id_rsa* = 600
ssh-keygen -t rsa -b 2048
# copy SSH key to a remote host. Provide remote host username and password
ssh-copy-id -i ~/.ssh/id_rsa_<provider>.pub <username>@<host>
# connect to the remote host using SSH key but not password. Provide passphrase
ssh -i ~/.ssh/id_rsa_<provider> <username>@<host>
# configure SSH key alias in ~/.ssh/config
Host <alias>
    HostName <host>
    User <user>
    IdentityFile ~/.ssh/id_rsa<provider>
# connect to the remote host using SSH key alias. Provide passphrase
ssh <alias>
```

Convert private key and certificate from PEM into JKS format:
```bash
cat key.pem cert.pem > key-cert.pem
# enter new export password: changeit
openssl pkcs12 -export -in key-cert.pem -out key-cert.p12
# provide source keystore password: changeit
# enter new destination keystore password: changeit
keytool -importkeystore -srckeystore key-cert.p12 -srcstoretype pkcs12 -destkeystore key-cert.jks
```
