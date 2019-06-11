#!/bin/bash
set -e

function die() {
  echo ${1}
  exit 254
}

function call_bk() {
  method=${1}
  path=${2}
  qs=""
  if [ ! -z ${3+x} ]; then
    qs=${3}
  fi
  url=$(bk_url ${path} ${qs})
  curl -sX ${method} "${url}"
}

function bk_url() {
  path=${1}
  qs=""
  if [ ! -z ${2+x} ]; then
    qs=${2}
  fi
  echo "https://api.buildkite.com/v2/organizations/adroll-group/${path}?access_token=${BUILDKITE_TOKEN}&${qs}"
}
