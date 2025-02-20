package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowStage;
import info.openmeta.starter.flow.service.FlowStageService;

/**
 * FlowStage Model Service Implementation
 */
@Service
public class FlowStageServiceImpl extends EntityServiceImpl<FlowStage, String> implements FlowStageService {

}