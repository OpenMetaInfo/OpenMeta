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

# Function to recursively update parent version in child modules
function update_child_modules {
  local module_dir="$1"
  local new_version="$2"

  find "$module_dir" -name "pom.xml" | while read -r pom_file; do
    local submodule
    submodule=$(dirname "$pom_file")
    if [ "$submodule" != "$module_dir" ]; then
      echo "Updating $submodule parent version to $new_version"
      mvn versions:update-parent -DparentVersion="$new_version" -DgenerateBackupPoms=false -f "$pom_file"
      update_child_modules "$submodule" "$new_version"
    fi
  done
}

# Start recursively updating all child modules' parent versions from the project root directory
update_child_modules "." "$NEW_VERSION"