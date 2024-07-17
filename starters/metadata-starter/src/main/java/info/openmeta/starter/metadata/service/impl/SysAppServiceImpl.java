package info.openmeta.starter.metadata.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysApp;
import info.openmeta.starter.metadata.service.SysAppService;

/**
 * SysApp Model Service Implementation
 */
@Service
public class SysAppServiceImpl extends EntityServiceImpl<SysApp, Long> implements SysAppService {

}