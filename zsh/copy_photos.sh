#!/usr/bin/env bash

set -eu

# Copy and organize photos into albums by date
# Usage: ./copy_photos.sh "/My photos" "/My albums"
function copy_photos {
    local source_path="${1:?ERROR: source path is not provided}"
    local destination_path="${2:?ERROR: destination path is not provided}"

    function copy_photo {
        local photo="${1:?ERROR: photo is not provided}"
        local destination_path="${2:?ERROR: destination path is not provided}"

        echo -n "${photo}"
        local timestamp=$(
            exiv2 "${photo}" \
            | grep -oP '(?<=Image timestamp : ).+$' | sed 's/:/-/;s/:/-/'
        )
        if [[ -z "${timestamp}" ]]; then
           echo " FAILED (no image timestamp)"
           return 1
        fi
        local date=$(date -d "${timestamp}" +'%Y-%m-%d')
        local time=$(date -d "${timestamp}" +'%H:%M:%S')
        if [[ -z "${date}" ]] || [[ -z "${time}" ]]; then
            echo " FAILED (invalid image timestamp ${timestamp})"
            return 1
        fi
        local hash=$(sha1sum "${photo}" | grep -oP '^\S+')
        local file="${date}_${time}_${hash}.jpg"
        local date_path=$(date -d "${timestamp}" +'%Y/%Y-%m-%d')
        local path="${destination_path}/${date_path}"
        mkdir -p "${path}"
        cp "${photo}" "${path}/${file}"
        echo " DONE"
    }
    export -f copy_photo

    find "${source_path}" -type f -iregex '.*\.\(jpg\|jpeg\)$' \
         -exec bash -c "copy_photo \"{}\" \"${destination_path}\"" \;
}

copy_photos "${@}"
