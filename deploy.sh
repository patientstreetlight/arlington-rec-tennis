#!/bin/sh

elm make src/main.elm
aws s3 cp index.html s3://chrisrice.io/index.html
