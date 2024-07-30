package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowTriggerRelation;
import info.openmeta.starter.flow.service.FlowTriggerRelationService;

/**
 * FlowTriggerRelation Model Service Implementation
 */
@Service
public class FlowTriggerRelationServiceImpl extends EntityServiceImpl<FlowTriggerRelation, Long> implements FlowTriggerRelationService {

}