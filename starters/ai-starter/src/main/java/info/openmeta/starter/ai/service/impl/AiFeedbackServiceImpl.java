package info.openmeta.starter.ai.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.ai.entity.AiFeedback;
import info.openmeta.starter.ai.service.AiFeedbackService;

/**
 * AiFeedback Model Service Implementation
 */
@Service
public class AiFeedbackServiceImpl extends EntityServiceImpl<AiFeedback, Long> implements AiFeedbackService {

}