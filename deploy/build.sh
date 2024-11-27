#!/bin/bash
# Build script for multi-platform Docker images
# Usage: ./deploy/build.sh <APP_PATH> <APP_VERSION>
# Example: ./deploy/build.sh ./apps/demo-app 0.3.2

# Check if sufficient arguments were provided
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <APP_PATH> <APP_VERSION>"
    exit 1
fi

# Define the application path and version from command-line arguments
APP_PATH=$1
APP_VERSION=$2

# Extract APP_DIR and APP_NAME from APP_PATH
APP_DIR=$(dirname "$APP_PATH")
APP_NAME=$(basename "$APP_PATH")

# Build multi-platform images and push them to Docker Hub
docker buildx build --platform linux/amd64,linux/arm64 \
    --build-arg APP_DIR=$APP_DIR \
    --build-arg APP_NAME=$APP_NAME \
    --build-arg APP_VERSION=$APP_VERSION \
    -t openmeta/${APP_NAME}:${APP_VERSION} \
    -f ./deploy/Dockerfile \
    --push .