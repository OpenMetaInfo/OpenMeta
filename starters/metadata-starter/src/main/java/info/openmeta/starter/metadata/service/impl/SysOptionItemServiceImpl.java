package info.openmeta.starter.metadata.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysOptionItem;
import info.openmeta.starter.metadata.service.SysOptionItemService;

/**
 * SysOptionItem Model Service Implementation
 */
@Service
public class SysOptionItemServiceImpl extends EntityServiceImpl<SysOptionItem, Long> implements SysOptionItemService {

}