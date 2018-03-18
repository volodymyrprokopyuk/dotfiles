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
docker stop postgres-test
docker rm postgres-test
# host: localhost, port: 5432, user: postgres, password: postgres-password, database: postgres
docker run --name postgres-test \
    -e POSTGRES_PASSWORD=postgres-password \
    -d -p 5432:5432 postgres
docker start postgres-test
docker ps
docker exec -it postgres-test bash
su - postgres
psql
CREATE DATABASE testdatabase;
CREATE USER testuser WITH ENCRYPTED PASSWORD 'testpassword';
GRANT ALL PRIVILEGES ON DATABASE testdatabase TO testuser;
# host: localhost, port: 5432, user: testuser, password: testpassword, database: testdatabase
psql -U testuser -d testdatabase
CREATE SCHEMA testschema;
CREATE TABLE IF NOT EXISTS testschema.testtable(id SERIAL, first_name TEXT, last_name TEXT, PRIMARY KEY (id));
INSERT INTO testschema.testtable(first_name, last_name) VALUES ('Volodymyr', 'Prokopyuk');
```

Install Keycloak:
```bash
# host: localhost, port: 8080, user: testuser, password: testpassword
docker run --name keycloak-test --net=host \
    -e KEYCLOAK_LOGLEVEL=DEBUG \
    -e KEYCLOAK_USER=testuser \
    -e KEYCLOAK_PASSWORD=testpassword \
    -e POSTGRES_ADDR=127.0.0.1 \
    -e POSTGRES_PORT=5432 \
    -e POSTGRES_DATABASE=testdatabase \
    -e POSTGRES_USER=testuser \
    -e POSTGRES_PASSWORD=testpassword \
    -d -p 8080:8080 jboss/keycloak
docker logs -f keycloak-test
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
