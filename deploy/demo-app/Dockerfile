# docker build -t openmeta/demo-app:0.3.2 -f ./deploy/demo-app/Dockerfile .

# Stage 1: Build app jar
FROM maven:3.9.8-eclipse-temurin-21-alpine as BUILD_STAGE

COPY ../.. /build/

WORKDIR /build/
#COPY ./.mvn/settings.xml /root/.m2/

RUN mvn package -pl apps/demo-app -Dmaven.test.skip=true

# Stage 2: Build final image with app jar
FROM eclipse-temurin:21-jre-alpine

ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime &&  echo $TZ > /etc/timezone

COPY --from=BUILD_STAGE /build/target/demo-app.jar /app/

WORKDIR /app/
ENTRYPOINT ["java", "-jar", "demo-app.jar"]