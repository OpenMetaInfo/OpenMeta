# docker build -t openmeta/demo-app:0.3.2 -f ./deploy/demo-app/Dockerfile .
# Define the app name
ARG APP_NAME=demo-app
ARG APP_VERSION=0.3.2

# Stage 1: Build app jar
#FROM maven:3.9.8-eclipse-temurin-21-alpine as BUILD_JAR
FROM openmeta/base-image:${APP_VERSION} as BUILD_JAR

ARG APP_NAME
COPY . /build/
WORKDIR /build/
RUN mvn package -pl apps/${APP_NAME} -Dmaven.test.skip=true

# Stage 2: Build final image with app jar
FROM eclipse-temurin:21-jre-alpine

ARG APP_NAME
ENV APP_NAME=${APP_NAME} \
    TZ=Asia/Shanghai

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

COPY --from=BUILD_JAR /build/apps/${APP_NAME}/target/${APP_NAME}.jar /app/
COPY ./deploy/entrypoint.sh /app/entrypoint.sh

WORKDIR /app
RUN chmod +x ./entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]