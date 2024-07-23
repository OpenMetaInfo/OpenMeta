
## Cron Scheduler Pool
Scheduler thread pool size, default is 1.

## Quartz Cron Expression

#### Format
```
* * * ? * * [*]
- - - - - -  -
| | | | | |  |
| | | | | |  +- Year              (OPTIONAL)
| | | | | +---- Day of the Week   (range: 1-7 or SUN-SAT, 1 standing for Monday)
| | | | +------ Month             (range: 1-12 or JAN-DEC)
| | | +-------- Day of the Month  (range: 1-31)
| | +---------- Hour              (range: 0-23)
| +------------ Minute            (range: 0-59)
+-------------- Second            (range: 0-59)
```

#### Special Characters
* (*) means match any
* (?) means no specific value
* (,) means value list separator, e.g. 1,2,3
* (-) means range of values, e.g. 1-5 means 1,2,3,4,5
* (/) means increments, e.g. 1/5 means 1,6,11,16,21,26...
