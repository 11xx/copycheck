#!/usr/bin/env sh

dir=.test-cases
mkdir -p "$dir"

cd "$dir" || printf 'Couldnt cd to %s, exiting...\n\n' "$dir" && exit 1

# remove echo for real creation
cat <<EOF | while read -r a; do touch "$a"; done
.hiddot.ext
.hid_nodot
filedot.ext
file_nodot
.hiddot_COPY_01.ext
.hid_nodot_COPY_01
filedot_COPY_01.ext
file_nodot_COPY_01
.hiddot_COPY_02.ext
.hid_nodot_COPY_02
filedot_COPY_02.ext
file_nodot_COPY_02
.hiddot_COPY_03.ext
.hid_nodot_COPY_03
filedot_COPY_03.ext
file_nodot_COPY_03
filename_specialçãõäüå.ext
EOF


# these files are dirtier looking but they all have the unique
# string `-QNAQNeQuoC' that can be easily matched by find

# cat <<EOF | while read -r a; do echo touch "$a"; done
# .hiddot-QNAQNeQuoC.ext
# .hid_nodot-QNAQNeQuoC
# filedot-QNAQNeQuoC.ext
# file_nodot-QNAQNeQuoC
# .hiddot-QNAQNeQuoC_COPY_01.ext
# .hid_nodot-QNAQNeQuoC_COPY_01
# filedot-QNAQNeQuoC_COPY_01.ext
# file_nodot-QNAQNeQuoC_COPY_01
# .hiddot-QNAQNeQuoC_COPY_02.ext
# .hid_nodot-QNAQNeQuoC_COPY_02
# filedot-QNAQNeQuoC_COPY_02.ext
# file_nodot-QNAQNeQuoC_COPY_02
# .hiddot-QNAQNeQuoC_COPY_03.ext
# .hid_nodot-QNAQNeQuoC_COPY_03
# filedot-QNAQNeQuoC_COPY_03.ext
# file_nodot-QNAQNeQuoC_COPY_03
# filename-QNAQNeQuoC_specialçãõäüå.ext
# EOF

