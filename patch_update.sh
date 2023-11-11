#!/usr/bin/env sh

# Script for bumping tag from: https://stackoverflow.com/a/49655809
Rscript ./patch_update.R
VERSION=`git describe --tags --abbrev=0 | awk -F. '{OFS="."; $NF+=1; print $0}'`
git tag -a $VERSION -m "patch update"
git push origin $VERSION
