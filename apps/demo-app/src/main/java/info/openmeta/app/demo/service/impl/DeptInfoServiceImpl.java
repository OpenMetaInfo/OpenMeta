package info.openmeta.app.demo.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.app.demo.entity.DeptInfo;
import info.openmeta.app.demo.service.DeptInfoService;

/**
 * DeptInfo Model Service Implementation
 */
@Service
public class DeptInfoServiceImpl extends EntityServiceImpl<DeptInfo, Long> implements DeptInfoService {

}