# regexgen
A primitive regex parser and matcher generator.

[Read more in this post](http://dobegin.com/)

## Build instructions

Open **regexgen.sln**, build solution.

## Usage

Run regexgen.exe to generate a C++ file:

    regexgen.exe <function name> <regex pattern>

Example: [regexgen_mymatcher.bat](regexgen_mymatcher.bat)

Use the generated function from your C++ code like so: [example of matching](regexmatchertest/regexmatchertest.cpp#L13)
