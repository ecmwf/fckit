# Features:
#
# - Keeps carriage returns (unlike [:space:]), so it works fine on Windows/DOS-style files.
# - Only worries about "normal" whitespace - If you have vertical tabs or such in your files it's probably intentional (test code or raw data).
# - Skips the .git and .svn VCS directories.
# - Only modifies files which file thinks is a text file.
# - Reports all paths which were skipped.
# - Works with any filename.

while IFS= read -r -d '' -u 9
do
    if [[ "$(file -bs --mime-type -- "$REPLY")" = text/* ]]
    then
        sed -i -e 's/[ \t]\+\(\r\?\)$/\1/;$a\' -- "$REPLY"
    else
        echo "Skipping $REPLY" >&2
    fi
done 9< <(find . \( -type d -regex '^.*/\.\(git\|svn\|hg\)$' -prune -false \) -o -type f -print0)

