repos=$(find . -name .git -type d | xargs dirname )

exclusions=""
for r in $repos
do
    exclusions="$exclusions -not -path \"$r/*\""
done


cmd="find $1 -type b,c,p,f,l,s $exclusions"

eval $cmd

echo "Ran: $cmd" >> findfiles.log
echo "Skipped repos: $repos" >> findfiles.log

#everything except directories

#todo honor .ignore files or some glob

#cat files.txt | while read line ; do sha256sum "$line"; sha1sum "$line"; done >> hashes.txt

