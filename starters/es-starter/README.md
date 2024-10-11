# Configuration
## ES index:
    spring.elasticsearch.index.changelog
## MQ Topic:
```yml
rocketmq:
  topics:
    change-log:
      topic: dev_demo_change_log
      persist-group: dev_demo_change_log_persist_group
```