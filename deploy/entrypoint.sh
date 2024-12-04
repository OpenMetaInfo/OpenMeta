#!/bin/sh
set -eu

# Extract the last part of the host's IPv4 address and set it as the node ID of TSID
IP=$(hostname -i | awk '{print $1}')
if [ -z "$IP" ]; then
    echo "Error: Failed to obtain IP address."
    exit 1
fi

TSIDCREATOR_NODE=$(echo $IP | awk -F. '{print $4}')
if [ -z "$TSIDCREATOR_NODE" ]; then
    echo "Error: Failed to extract node ID from IP address."
    exit 1
fi

export TSIDCREATOR_NODE
# Set the number of nodes in the TSID cluster
export TSIDCREATOR_NODE_COUNT=256

# Now execute the main command of the container
exec java -Dspring.profiles.active="${SPRING_PROFILES_ACTIVE}" -jar "${APP_NAME}.jar" "$@"