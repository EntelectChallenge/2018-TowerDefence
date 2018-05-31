Requires MinGW
https://nuwen.net/files/mingw/mingw-15.4.exe is a nicely maintained distro with a few other tools but there are others.

Extracted the above files to C:\

compile.bat needs to be updated to your local MinGW path

C++ Source code: samplebot.cpp 
Compile with : compile.bat
	- Do not add full path too g++ compiler in compile.bat, for bot uploads. E.g.
		g++.exe -Wall samplebot.cpp -o samplebot.exe
Run bot : samplebot.exe

I'm using the TextMap.txt, haven't used json before.

Assumptions
 - No inputs are given to the bot at runtime
 - The working directory contains TextMap.txt
 - PlayerCommand.txt will be written to the working directory

Still to do
 - Remove random moves and add simple logic
 - Read in from json file

bot.json file
 - might need to have values for compile and running
