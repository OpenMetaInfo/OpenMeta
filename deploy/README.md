
# 1. Build multi-platform images and push them to Docker Hub
Specify the `APP_NAME`, `APP_VERSION` to build the image for the Java application.
```
./deploy/build.sh <APP_NAME> <APP_VERSION>
```
The app must be in the `./apps` directory, and the APP_NAME should be equal to the subdirectory name.

Example to build the demo application image:
```bash
./deploy/build.sh demo-app 0.3.2
```

# 2. Start the demo application by Docker Compose
```bash
docker-compose -f ./deploy/demo-app/docker-compose.yml up -d
```