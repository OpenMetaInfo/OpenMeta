package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysField;
import info.openmeta.starter.metadata.service.SysFieldService;
import org.springframework.stereotype.Service;

/**
 * SysField Model Service Implementation
 */
@Service
public class SysFieldServiceImpl extends EntityServiceImpl<SysField, Long> implements SysFieldService {

}