package info.openmeta.starter.flow.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowApproval;
import info.openmeta.starter.flow.service.FlowApprovalService;

/**
 * FlowApproval Model Service Implementation
 */
@Service
public class FlowApprovalServiceImpl extends EntityServiceImpl<FlowApproval, Long> implements FlowApprovalService {

}