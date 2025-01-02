package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowTrigger;
import info.openmeta.starter.flow.service.FlowTriggerService;

/**
 * FlowTrigger Model Service Implementation
 */
@Service
public class FlowTriggerServiceImpl extends EntityServiceImpl<FlowTrigger, String> implements FlowTriggerService {

}