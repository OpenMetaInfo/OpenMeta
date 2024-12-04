
# 1. Build multi-platform images and push them to Docker Hub
Specify the `APP_PATH`, `APP_VERSION` to build the image for the Java application.
```
./deploy/build.sh <APP_PATH> <APP_VERSION> [<REGISTRY_NAMESPACE>]
```
The `APP_PATH` is the relative path to the application source code directory, such as `apps/demo-app`.
And the last name in `APP_PATH` is the application name, such as `demo-app`.

The `REGISTRY_NAMESPACE` is optional, and the default value is `openmeta`. 
Your can specify the `REGISTRY_NAMESPACE` to push the image to your own docker image repository.

Example to build the demo application image:
```bash
./deploy/build.sh apps/demo-app 0.7.3
```

# 2. Start the demo application by Docker Compose
```bash
docker-compose -f ./deploy/demo-app/docker-compose.yml up -d
```

# 3. Start the EFK stack by Docker Compose
```bash
docker-compose -f deploy/efk/docker-compose.yml up -d
```
Access the Kibana console at http://localhost:5601

# 4. Start the RocketMQ stack by Docker Compose
```bash
docker-compose -f deploy/rocketmq/docker-compose.yml up -d
```
Access the RocketMQ console at http://localhost:8080

# 5. Start the Minio stack by Docker Compose
```bash
docker-compose -f deploy/minio/docker-compose.yml up -d
```
### Minio API Endpoints
http://localhost:9000

### Minio Web UI Dashboard
http://localhost:9001
Username: minioadmin
Password: minioadmin