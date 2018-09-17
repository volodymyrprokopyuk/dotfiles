# Installation

```bash
# install yay with makepkg from git
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
# install software with pacman from core, extra and community repositories
sudo pacman-mirrors --fasttrack 5 && pacman -Syyu
pacman -S adobe-source-code-pro-fonts tmux emacs the_silver_searcher fzf xsel diff-so-fancy mlocate
pacman -S ansible docker
pacman -S xmlstarlet jq pwgen ipcalc openbsd-netcat sysstat iotop ethtool
pacman -S intellij-idea-ce dbeaver-ce
# install software with yay from AUR
yay -S dropbox google-chrome skypeforlinux-preview-bin
yay -S apg yamllint
# configure zsh
chsh -s $(which zsh)
# configure docker
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker
sudo systemctl start docker
# logout, login and then configure docker swarm
docker swarm init
```
# pacman / yay usage:

```bash
# update the repository database
sudo pacman -Sy
# upgrade the system
sudo pacman -Syu
# search the remote repository database for a package
pacman -Ss[i] <package>
# query the local repository database for the installed package
pacman -Qs[i] <package>
# install a package
sudo pacman -S <package>
# remove a package, its configuration and dependencies
sudo pacman -Rsn <package>
# list all files owned by a package
pacman -Ql <package>
# show which package the file belongs to
pacman -Qo <file path>
# show dependency tree of a package
pactree <package>
# show system statistics
yay -Ps
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

Install and use locally generated SSH key on a remote server:
```bash
# generate SSH key pair locally. Provide SSH key file location ($HOME/.ssh/id_rsa_<provider>) and passphrase
# file parmissions: ~/.ssh = 700, ~/.ssh/id_rsa* = 600
ssh-keygen -t rsa -b 4096 -C "volodymyrprokopyuk@gmail.com"
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

Add private SSH key identities to the SSH authentication agent
```bash
# start SSH authentication agent
eval $(ssh-agent)
# add private SSH key identities to the SSH authentication agent. Provide passphrase
ssh-add ~/.ssh/id_rsa*~*.pub
# show added to the SSH authentication agent private SSH key identities
ssh-add -L
```

Show local / remote PEM certificate details:
```bash
# local file
openssl x509 -inform pem -noout -text -in cert.pem
# remote host
echo | openssl s_client -showcerts -servername <host> -connect <host>:<port> 2>/dev/null | openssl x509 -inform pem -noout -text
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

Show JKS details:
```bash
keytool -v -list -keystore key-cert.jks
```

Import certificate into cacerts:
```bash
keytool -import -trustcacerts -keystore $JAVA_HOME/jre/lib/security/cacerts -storepass <changeit> -noprompt -alias <alias> -file cert.pem
```
