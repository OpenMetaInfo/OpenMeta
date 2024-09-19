# Configuration
## MQ Topic:
```yml
rocketmq:
  topics:
    change-log:
      topic: dev_demo_change_log
      flow-group: dev_demo_change_log_flow_group
    cron-task:
      topic: dev_demo_cron_task
      flow-group: dev_demo_cron_task_flow_group
    flow-async-task:
      topic: dev_demo_flow_async_task
      group: dev_demo_flow_async_task_group
    flow-event:
      topic: dev_demo_flow_event
      group: dev_demo_flow_event_group
```