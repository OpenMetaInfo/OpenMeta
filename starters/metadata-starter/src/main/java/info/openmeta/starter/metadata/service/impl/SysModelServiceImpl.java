package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysModel;
import info.openmeta.starter.metadata.service.SysModelService;
import org.springframework.stereotype.Service;

/**
 * SysModel Model Service Implementation
 */
@Service
public class SysModelServiceImpl extends EntityServiceImpl<SysModel, Long> implements SysModelService {

}