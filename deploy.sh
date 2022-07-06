#!/bin/sh

set -x

for file in $(find site -type f) ; do
    base_file=$(echo $file | sed 's,^site/,,')
    aws s3 cp ${file} s3://chrisrice.io/${base_file}
done
