#!/bin/bash

mkdir tmp
cd tmp
git clone git@github.com:dfalster/baad.git 
cd baad
remake

cd ..
git clone git@github.com:RemkoDuursma/baadanalysis.git 
cd baadanalysis
remake

cd ..
git clone git@github.com:dfalster/building_baad.git
cd building_baad
remake
