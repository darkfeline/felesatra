set -eu
cd app/srv/goproxy || exit 1
readonly host=https://proxy.golang.org
find * -name "*.info" -printf "-s ${host}/%p\n" | xargs -P 0 -L 1 curl -w '\n'
echo
