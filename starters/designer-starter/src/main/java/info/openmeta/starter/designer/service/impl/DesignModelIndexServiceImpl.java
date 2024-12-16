package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignModelIndex;
import info.openmeta.starter.designer.service.DesignModelIndexService;

/**
 * DesignModelIndex Model Service Implementation
 */
@Service
public class DesignModelIndexServiceImpl extends EntityServiceImpl<DesignModelIndex, Long> implements DesignModelIndexService {

}