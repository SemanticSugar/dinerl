#!/bin/bash
set -ue

root=$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )/../..
. ${root}/pipelines/common.sh

if [ "${BUILDKITE_BRANCH}" = "main" ]; then
cat <<EOF
- label: "Trigger builds"
  agents:
    queue: 'rtb-new'
  command:
    - "echo Triggering builds"
EOF
  # If the pipeline was triggered because of a push to main, we want to
  # rebuild all the pull requests to see if they will still work with these
  # new changes (merge + checks + tests).
  #
  # Get all the open pull requests from GitHub and find their statuses_url.
  build_regex="/builds/([0-9]+)"
  results=$(curl \
    -s \
    -H "Content-Type: application/json" \
    -H "Authorization: token ${GITHUB_TOKEN}" \
    "https://api.github.com/repos/SemanticSugar/dinerl/pulls?per_page=100&state=open" \
    | jq -r '.[] | [.number,.statuses_url] | @tsv' \
  )

  # Find all the builds for this pipeline for each pull request. Analyze
  # (i.e: regex) all the target_urls and find the latest build (i.e: the
  # build with the highest number), this will be the build that we will
  # trigger.
  IFS=$'\n'
  for result in ${results}; do
    IFS=$'\t' read pr_number status <<< "${result}"
    builds=$(curl \
      -s \
      -H "Content-Type: application/json" \
      -H "Authorization: token ${GITHUB_TOKEN}" \
      ${status} \
      | jq '.[] | select(.target_url | contains("dinerl-merge-builder")) | [.target_url] | @tsv' \
    )
    latest_build=0
    for build in ${builds}; do
      [[ ${build} =~ ${build_regex} ]]
      build_number=${BASH_REMATCH[1]}
      if [ ${build_number} -gt ${latest_build} ]; then
        latest_build=${build_number}
      fi
    done
    # If we found a build, trigger a rebuild.
    if [ "${latest_build}" -gt 0 ]; then
cat <<EOF
    - "$(bk_url pipelines/dinerl-merge-builder/builds/${latest_build}/rebuild)"
EOF
    fi
  done
else
  # Read the pipeline and output replacing variables
  sed -e "s/\$MERGE_BRANCH/main/g" ${root}/pipelines/dinerl-pr-builder/start.yml
fi
