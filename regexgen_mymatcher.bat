@echo off
cd regexgen\bin\Debug
regexgen.exe matchString "(a|b)*abb" > ..\..\..\regexmatchertest\mymatcher.cpp
