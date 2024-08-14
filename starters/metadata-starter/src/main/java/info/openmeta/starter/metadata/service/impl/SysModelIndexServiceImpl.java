package info.openmeta.starter.metadata.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysModelIndex;
import info.openmeta.starter.metadata.service.SysModelIndexService;

/**
 * SysModelIndex Model Service Implementation
 */
@Service
public class SysModelIndexServiceImpl extends EntityServiceImpl<SysModelIndex, Long> implements SysModelIndexService {

}