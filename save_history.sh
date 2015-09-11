#!/bin/bash
cat $APPDATA/ghc/ghci_history > .tmp && cat ghci_history >> .tmp && mv .tmp ghci_history
g a ghci_history
g c "save history"

