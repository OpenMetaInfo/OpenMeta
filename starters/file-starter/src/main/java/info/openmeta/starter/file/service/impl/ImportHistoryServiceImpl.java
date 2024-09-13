package info.openmeta.starter.file.service.impl;

import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.service.ImportHistoryService;
import org.springframework.stereotype.Service;

/**
 * ImportHistory service implementation
 */
@Service
public class ImportHistoryServiceImpl extends EntityServiceImpl<ImportHistory, Long> implements ImportHistoryService {

}