#!/bin/bash

swift switch-ime.swift com.apple.keylayout.Dvorak
pkill -f "BoGo.app"
sleep 2
rm -rf ~/Library/Input\ Methods/BoGo.app

cp -r "/Users/lewtds/Library/Developer/Xcode/DerivedData/BoGo-dyevlllcsckdzzamzlpwupbunlaa/Build/Products/Debug/BoGo.app" ~/Library/Input\ Methods
# sleep 2
# swift switch-ime.swift com.lewtds.inputmethod.BoGo
