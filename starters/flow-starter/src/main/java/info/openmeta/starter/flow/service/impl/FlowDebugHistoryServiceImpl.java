package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowDebugHistory;
import info.openmeta.starter.flow.service.FlowDebugHistoryService;

/**
 * FlowDebugHistory Model Service Implementation
 */
@Service
public class FlowDebugHistoryServiceImpl extends EntityServiceImpl<FlowDebugHistory, Long> implements FlowDebugHistoryService {

}