package info.openmeta.app.demo.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.app.demo.entity.ProjectInfo;
import info.openmeta.app.demo.service.ProjectInfoService;

/**
 * ProjectInfo Model Service Implementation
 */
@Service
public class ProjectInfoServiceImpl extends EntityServiceImpl<ProjectInfo, Long> implements ProjectInfoService {

}