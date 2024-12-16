package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignOptionSet;
import info.openmeta.starter.designer.service.DesignOptionSetService;

/**
 * DesignOptionSet Model Service Implementation
 */
@Service
public class DesignOptionSetServiceImpl extends EntityServiceImpl<DesignOptionSet, Long> implements DesignOptionSetService {

}