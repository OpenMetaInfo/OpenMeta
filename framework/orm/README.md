# Configuration
## MQ Topic:
```yml
rocketmq:
  topics:
    change-log:
      topic: rocketmq.topics.change-log
```

## Multi-Datasource
Application Scenarios:
* Read-write separation
* Operate multiple databases in one project

### Multiple Datasource Configuration
The multi-datasource is enabled by configuring `spring.datasource.dynamic.enable = true`.
Otherwise, using the original `spring.datasource.*` as the single datasource.

The first datasource is the default datasource when not specified. 
The datasource name can be customized in the `application.yml` file.
```yml
spring:
  datasource:
    dynamic:
      enabled: true
      datasource:
        primary:
          driver-class-name: com.mysql.cj.jdbc.Driver
          url: jdbc:mysql://localhost:3306/demo
          username: user0
          password: pass0
        db1:
          driver-class-name: com.mysql.cj.jdbc.Driver
          url: jdbc:mysql://localhost:3306/db1
          username: user1
          password: pass1
        db2:
          driver-class-name: com.mysql.cj.jdbc.Driver
          url: jdbc:mysql://localhost:3306/db2
          username: user2
          password: pass2
```

### Specify the datasource in Java code
The name of the datasource is the same as the key in the `application.yml` file.
```java
@DataSource("db1")
public void method1() {
    // ...
}
```
Datasource propagation mechanism:
* If the method does not have the DataSource annotation, get the class level annotation.
* If the previous datasource is the same as the current datasource, no need to switch.
* If the previous datasource is different from the current datasource, throw an exception.
* If the previous datasource is null, set the specified datasource as the current datasource.
* If the datasource is set firstly, it will be cleared after the method is executed.

### Deal with the problem of read-after-write consistency
In the read-write separation scenario, and non-transactional context environment, the read-after-write consistency problem may occur.
The solution is to use the `@DataSource` annotation to specify the datasource for the read operation.
```java
// When 'primary' is the write datasource.
@DataSource("primary")
public void readMethod1() {
    // ...
}
```