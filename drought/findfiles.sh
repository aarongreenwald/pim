gitrepos=$(find . -name .git -type d | xargs --no-run-if-empty dirname )
svnrepos=$(find . -name trunk -type d | xargs --no-run-if-empty dirname )

exclusions=""
for r in $gitrepos
do
    exclusions="$exclusions -not -path \"$r/*\""
done

for r in $svnrepos
do
    exclusions="$exclusions -not -path \"$r/*\""
done

cmd="find $1 -type b,c,p,f,l,s $exclusions"

eval $cmd

echo """
========================
$(date)
========================
Ran: $cmd in $(pwd)

Skipped repos:
$svnrepos
$gitrepos
""" >> ../findfiles.log

#everything except directories

#todo honor .ignore files or some glob

#cat files.txt | while read line ; do sha256sum "$line"; sha1sum "$line"; done >> hashes.txt

