#!/bin/bash

# Upgrade the version of all maven modules to the specified target version.
# Usage: ./upgrade-versions.sh <target-version>
# Example: ./upgrade-versions.sh 1.0.0

# Check if a new version number is provided as an argument
if [ -z "$1" ]; then
  read -p "Please enter the new version number: " NEW_VERSION
else
  NEW_VERSION=$1
fi

# Ensure the new version number is not empty
if [ -z "$NEW_VERSION" ]; then
  echo "No version number provided. Exiting."
  exit 1
fi

# Use regular expressions to check if the version number is valid
if [[ ! "$NEW_VERSION" =~ ^[0-9]+(\.[0-9]+)*$ ]]; then
  echo "Invalid version number. It should only contain numbers and dots. Such as 1.0.0"
  echo "Exiting."
  exit 1
fi

# Update the parent POM file version
echo "Updating parent POM version to $NEW_VERSION"
mvn versions:set -DnewVersion="$NEW_VERSION"
mvn versions:commit

# update the openmeta.version property in all child modules under ./apps
function update_apps_submodules {
  local apps_dir="$1"
  local new_version="$2"

  echo "Updating openmeta.version in all child modules under $apps_dir to $new_version ..."

  find "$apps_dir" -mindepth 2 -name "pom.xml" | while read -r apps_child_pom; do
    local child_dir
    child_dir=$(dirname "$apps_child_pom")

    echo "  -> Setting openmeta.version in $apps_child_pom"
    mvn versions:set-property \
        -Dproperty=openmeta.version \
        -DnewVersion="$new_version" \
        -DgenerateBackupPoms=false \
        -f "$apps_child_pom"

    update_apps_submodules "$child_dir" "$new_version"
  done
}

# update the project version in framework and starters modules
function update_child_modules {
  local module_dir="$1"
  local new_version="$2"

  find "$module_dir" -name "pom.xml" | while read -r pom_file; do
    local submodule
    submodule=$(dirname "$pom_file")

    # skip the current module directory
    if [ "$submodule" == "$module_dir" ]; then
      continue
    fi

    # Update the openmeta.version property in all child modules under ./apps
    if [[ "$submodule" == "./apps"* ]]; then
      echo "Skipping parent version update for $submodule"
      update_apps_submodules "$submodule" "$new_version"
      continue
    fi

    # Update the parent version in the current submodule
    echo "Updating $submodule parent version to $new_version"
    mvn versions:update-parent -DparentVersion="$new_version" -DgenerateBackupPoms=false -f "$pom_file"

    # Recursively update child modules
    update_child_modules "$submodule" "$new_version"
  done
}

# Start recursively updating all child modules' parent versions from the project root directory
update_child_modules "." "$NEW_VERSION"