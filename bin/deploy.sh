#!/bin/sh

for file in $(find site -type f) ; do
    base_file=$(echo $file | sed 's,^site/,,')
    set -x
    aws s3 cp ${file} s3://chrisrice.io/${base_file}
    set +x
done
