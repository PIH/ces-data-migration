#!/bin/bash
#
# Extracts the Roxygen package documentation from each script file in the 
# package root directory and appends it to README.md
#

OUTFILE="README.md"

r_files=( *.R )

echo >>${OUTFILE}
echo '## Script Documentation' >>${OUTFILE}
echo >>${OUTFILE}

for file in "${r_files[@]}"
do
  echo '### '${file} >>${OUTFILE}
  cat ${file} | \
    grep "@docType package" -B100 | \
    grep -v "@docType package" | \
    sed "s/^.\{0,3\}//" | \
    tail -n +2 \
    >> ${OUTFILE}
  echo >>${OUTFILE}
  cat ${file} | \
    grep "OUTPUT_PATH = " | \
    sed "s/.*/- \`&\`/" \
    >> ${OUTFILE}
  echo >>${OUTFILE}
done
