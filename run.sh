#!/bin/sh
#kill `cat ./my.term.pid`
loc="$PWD"
cd "$loc"



gnome-terminal -e 'bash -c "service mongod start; exec bash"'
sleep 5
gnome-terminal -e 'bash -c "service neo4j start; exec bash"'
sleep 5
gnome-terminal -e 'bash -c "cd '$loc'/Crawler;echo $PPID;stack exec Crawler-exe; exec bash"'
sleep 5
gnome-terminal -e 'bash -c "cd '$loc'/Search;echo $PPID;stack exec Search-exe; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/Lab1;echo $PPID;stack exec Lab1; exec bash"' 
