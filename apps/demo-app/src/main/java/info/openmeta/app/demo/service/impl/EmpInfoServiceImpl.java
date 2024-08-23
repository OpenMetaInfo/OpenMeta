package info.openmeta.app.demo.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.app.demo.entity.EmpInfo;
import info.openmeta.app.demo.service.EmpInfoService;

/**
 * EmpInfo Model Service Implementation
 */
@Service
public class EmpInfoServiceImpl extends EntityServiceImpl<EmpInfo, Long> implements EmpInfoService {

}