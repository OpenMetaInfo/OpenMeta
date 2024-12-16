package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignOptionItem;
import info.openmeta.starter.designer.service.DesignOptionItemService;

/**
 * DesignOptionItem Model Service Implementation
 */
@Service
public class DesignOptionItemServiceImpl extends EntityServiceImpl<DesignOptionItem, Long> implements DesignOptionItemService {

}