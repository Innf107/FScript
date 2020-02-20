sudo mkdir -p /etc/fscript/modules
sudo cp -r stdlib /etc/fscript/modules/stdlib
echo "Installed stdlib into /etc/fscript/modules/stdlib"
sudo cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/FScriptII-exe/FScriptII-exe /usr/local/bin/fscript
echo "Installed executable into /usr/local/bin/fscript"