#!/bin/bash
# Build script for multi-platform Docker images
# Example: ./deploy/build.sh demo-app 0.3.2

# Check if sufficient arguments were provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <APP_NAME> <APP_VERSION>"
    exit 1
fi

# Define the application name and version from command-line arguments
APP_NAME=$1
APP_VERSION=$2

# Build multi-platform images and push them to Docker Hub
docker buildx build --platform linux/amd64,linux/arm64 \
    --build-arg APP_NAME=$APP_NAME \
    --build-arg APP_VERSION=$APP_VERSION \
    -t openmeta/${APP_NAME}:${APP_VERSION} \
    -f ./deploy/Dockerfile \
    --push .