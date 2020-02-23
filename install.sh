sudo mkdir -p /etc/fscript/modules
sudo cp -r stdlib /etc/fscript/modules/stdlib
echo "Installed stdlib into /etc/fscript/modules/stdlib"
sudo touch /etc/fscript/history.txt
sudo chmod o=rw /etc/fscript/history.txt
echo "Installed repl history file into /etc/fscript/modules/history.txt"
sudo cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/FScriptII-exe/FScriptII-exe /usr/local/bin/fscript
echo "Installed x86_64-linux-tinfo6 executable into /usr/local/bin/fscript"

