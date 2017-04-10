#!/bin/sh
loc="$PWD"

cd "$loc/CommonResources"
stack setup
stack build
echo "Building Crawler..."
cd "$loc/Crawler"
stack setup
stack build
echo "Building Search Api..."
cd  "$loc/Search"
stack setup
stack build
echo "Building Web Service..."
cd  "$loc/Lab1"
stack setup
stack build
