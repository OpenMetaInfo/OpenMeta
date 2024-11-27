#!/bin/bash
# Build script for multi-platform Docker images with namespace and registry
# Usage: ./deploy/build.sh <APP_PATH> <APP_VERSION> [<REGISTRY_NAMESPACE>]
# Example: ./deploy/build.sh ./apps/demo-app 0.3.2 registry.example.com/openmeta

# Check if sufficient arguments were provided
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <APP_PATH> <APP_VERSION> [<REGISTRY_NAMESPACE>]"
    exit 1
fi

# Define the application path, version, and registry with namespace from command-line arguments
APP_PATH=$1
APP_VERSION=$2
REGISTRY_NAMESPACE=${3:-docker.io/openmeta} # Default to Docker Hub's openmeta namespace

# Extract APP_DIR and APP_NAME from APP_PATH
APP_DIR=$(dirname "$APP_PATH")
APP_NAME=$(basename "$APP_PATH")

# Full image name
IMAGE_NAME=${REGISTRY_NAMESPACE}/${APP_NAME}:${APP_VERSION}

# Display configuration for the build
echo "Building image:"
echo "  Application Path: $APP_PATH"
echo "  Application Version: $APP_VERSION"
echo "  Registry and Namespace: $REGISTRY_NAMESPACE"
echo "  Image Name: $IMAGE_NAME"

# Build multi-platform images and push them to Docker Hub
docker buildx build --platform linux/amd64,linux/arm64 \
    --build-arg APP_DIR=$APP_DIR \
    --build-arg APP_NAME=$APP_NAME \
    --build-arg APP_VERSION=$APP_VERSION \
    -t $IMAGE_NAME \
    -f ./deploy/Dockerfile \
    --push .