#!/bin/sh

# Get the machine set up

echo "Begin setup"
cd ~

sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y build-essential fortune-mod fortunes cowsay


# Install Node

mkdir ~/local
echo 'export PATH=$HOME/local/bin:$PATH' >> ~/.bashrc
. ~/.bashrc

git clone git://github.com/joyent/node.git
cd node
git checkout '0.8.4'
./configure --prefix=~/local
make install
cd ..

git clone git://github.com/isaacs/npm.git
cd npm
make install


# Now set up my environment

cd hipchat-fortune-moo
npm install

echo "To finish the setup process, you need to create your config.js file FIRST."
echo "To start the app, run 'NODE_ENV=production node app.js'."

