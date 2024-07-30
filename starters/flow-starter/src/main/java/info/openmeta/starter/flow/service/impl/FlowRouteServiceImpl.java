package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowRoute;
import info.openmeta.starter.flow.service.FlowRouteService;

/**
 * FlowRoute Model Service Implementation
 */
@Service
public class FlowRouteServiceImpl extends EntityServiceImpl<FlowRoute, Long> implements FlowRouteService {

}