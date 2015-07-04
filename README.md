transform-tree
==============

transform-tree allows for replicating a directory tree, while applying various transformations to its contents.

Files can be copied or linked to the originals, and both files and directories can be renamed. Files can also be converted using an external command.

Examples
--------

Create renamed links, removing disallowed characters so files can be shared over a network without name mangling:

    transform-tree -s -r 'tr "?:\42" "¬;\47"' /path/to/files /path/to/share

Convert only FLAC files in a file hierarchy to OGG format, preserving folder structure:

     transform-tree \
        -F '\.flac$' -r 'sed s/\.flac$/.ogg/' \
        -c 'avconv -y -i - -f ogg -acodec libvorbis -aq 6 -vn {out}' \
        --prune \
        /path/to/music /path/to/dest
