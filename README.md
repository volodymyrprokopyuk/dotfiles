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
sudo apt-get install git silversearcher-ag htop xmlstarlet jq
```

Install Ansible:

```bash
sudo apt-get install openssh-server
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys
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

Install PostgreSQL:
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

Install Keycloak:
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
