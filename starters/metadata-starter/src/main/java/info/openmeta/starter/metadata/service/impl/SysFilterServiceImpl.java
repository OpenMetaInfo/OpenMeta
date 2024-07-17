package info.openmeta.starter.metadata.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysFilter;
import info.openmeta.starter.metadata.service.SysFilterService;

/**
 * SysFilter Model Service Implementation
 */
@Service
public class SysFilterServiceImpl extends EntityServiceImpl<SysFilter, Long> implements SysFilterService {

}