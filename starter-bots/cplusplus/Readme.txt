Requires MinGW
https://nuwen.net/files/mingw/mingw-15.4.exe is a nicely maintained distro with a few other tools but there are others.

Extracted the above files to C:\

C++ Source code: samplebot.cpp 
Compile with : compile.bat
Run bot : samplebot.exe

I'm using the TextMap.txt, haven't used json before.

Assumptions
 - No inputs are given to the bot at runtime
 - The working directory contains TextMap.txt
 - PlayerCommand.txt will be written to the working directory

Still to do
 - Remove random moves and add simple logic
 - Read in from json file
