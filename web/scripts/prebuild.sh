set -e

rm -rf ./build

#because I don't want to clutter the commit log with version bumps, I need
#to get the latest version and set the local version to that so that the
#npm version command bumps to the write version.

#I should use jq but I ran into issues installing it, so for now this hacky sed
#if the spacing changes it'll break, as well as if I bump the minor or major
version=$(npm view @aarongreenwald/pim-web version)
sed -i "s/\"version\": \"1.0.0\"/\"version\": \"$version\"/" package.json