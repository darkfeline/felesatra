set -eu
cd goproxy/static || exit 1
readonly host=https://proxy.golang.org
find * -name "*.info" -exec curl "${host}/{}" \; >/dev/null
