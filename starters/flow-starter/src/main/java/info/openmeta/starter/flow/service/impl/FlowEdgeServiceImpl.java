package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowEdge;
import info.openmeta.starter.flow.service.FlowEdgeService;

/**
 * FlowEdge Model Service Implementation
 */
@Service
public class FlowEdgeServiceImpl extends EntityServiceImpl<FlowEdge, Long> implements FlowEdgeService {

}