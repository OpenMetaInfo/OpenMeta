package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowEvent;
import info.openmeta.starter.flow.service.FlowEventService;

/**
 * FlowEvent Model Service Implementation
 */
@Service
public class FlowEventServiceImpl extends EntityServiceImpl<FlowEvent, Long> implements FlowEventService {

}