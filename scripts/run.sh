#!/bin/bash

set -eu

function usage_exit () {
    docker-compose run --rm app stack exec ImagePacker -- --help
    exit 1
}

opts=()

while getopts ":?hs:m:v:-:"  OPT
do
    case $OPT in
        '?'|h)
            usage_exit
            ;;
        s)
            opts+=( "-s${OPTARG}" )
            ;;
        m)
            opts+=( "-m${OPTARG}" )
            ;;
        v)
            opts+=( "-v${OPTARG}" )
            ;;
        -)
            case "${OPTARG}" in
                input-extension=*|texture-size=*|metadata-settings=*|texture-filename=*|spacing=* )
                    opts+=( "--${OPTARG}" )
                    ;;
                * )
                    echo "unknown option: --$OPTARG" >&2
                    usage_exit
                    ;;
            esac
            ;;
        ?|*)
            echo "unknown option: $OPT" >&2
            usage_exit
    esac
done

shift $((OPTIND - 1))

if [ "$#" -lt 2 ]; then
    echo 'not enough arguments' >&2
    usage_exit
fi

input_path="$1"
output_path="$2"

if [ ! -d "$input_path" ]; then
    echo "input path ($input_path) is not a directory" >&2
    exit;
fi

if [ ! -d "$output_path" ]; then
    mkdir "$output_path"
fi

docker-compose run --rm \
    -v "$(realpath "$input_path"):/tmp/input" \
    -v "$(realpath "$output_path"):/tmp/output" \
    app stack exec ImagePacker -- "${opts[@]}" "/tmp/input" "/tmp/output"
