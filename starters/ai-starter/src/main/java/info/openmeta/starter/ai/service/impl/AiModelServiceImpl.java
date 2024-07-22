package info.openmeta.starter.ai.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.ai.entity.AiModel;
import info.openmeta.starter.ai.service.AiModelService;

/**
 * AiModel Model Service Implementation
 */
@Service
public class AiModelServiceImpl extends EntityServiceImpl<AiModel, Long> implements AiModelService {

}