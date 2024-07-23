package info.openmeta.starter.cron.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.cron.entity.SysCronLog;
import info.openmeta.starter.cron.service.SysCronLogService;

/**
 * SysCronLog Model Service Implementation
 */
@Service
public class SysCronLogServiceImpl extends EntityServiceImpl<SysCronLog, Long> implements SysCronLogService {

}