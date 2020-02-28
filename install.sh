if [ "$FSPATH" = "" ]
  then
    FSPATH=$HOME/.fscript
    echo "Set \$FSPATH to $FSPATH as it was unset."
fi

mkdir -p "$FSPATH/modules"
cp -r stdlib "$FSPATH/modules/"
echo "Installed stdlib into $FSPATH/modules/stdlib"
touch "$FSPATH/history.txt"
chmod o=rw "$FSPATH/history.txt"
echo "Installed repl history file into $FSPATH/modules/history.txt"
cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/FScriptII-exe/FScriptII-exe "$HOME/.local/bin/fscript"
echo "Installed x86_64-linux-tinfo6 executable into $HOME/.local/bin/fscript"

