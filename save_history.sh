#!/bin/bash
cat $APPDATA/ghc/ghci_history > .tmp && cat ghci_history >> .tmp && mv .tmp ghci_history
git add ghci_history
git commit -m "save history"

echo "" > $APPDATA/ghc/ghci_history
