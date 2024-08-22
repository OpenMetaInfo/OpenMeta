# Build base image with 3rd dependencies stored in /root/.m2
FROM maven:3.9.8-eclipse-temurin-21-alpine as BASE_IMAGE
COPY . /build/
WORKDIR /build/
RUN mvn dependency:go-offline