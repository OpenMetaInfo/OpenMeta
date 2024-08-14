package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowInstance;
import info.openmeta.starter.flow.service.FlowInstanceService;

/**
 * FlowInstance Model Service Implementation
 */
@Service
public class FlowInstanceServiceImpl extends EntityServiceImpl<FlowInstance, Long> implements FlowInstanceService {

}