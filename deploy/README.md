# 1. Build the base image for Java applications
Specify the version of the base image. 

The version of the base image should be the same as the version of the Java application.
```bash
docker build -t openmeta/base-image:0.3.2 -f ./deploy/Base.Dockerfile .
```


# 2. Build the image for the Java application
Specify the APP_NAME, APP_VERSION, and the tag name.

The app must be in the ./apps directory, and the APP_NAME should be equal to the subdirectory name.
```bash
docker build  --build-arg APP_NAME=demo-app \
              --build-arg APP_VERSION=0.3.2 \
              -t openmeta/demo-app:0.3.2 \
              -f ./deploy/Dockerfile .
```