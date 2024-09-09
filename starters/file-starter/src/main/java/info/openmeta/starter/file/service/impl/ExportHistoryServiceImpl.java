package info.openmeta.starter.file.service.impl;

import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.file.entity.ExportHistory;
import info.openmeta.starter.file.service.ExportHistoryService;
import org.springframework.stereotype.Service;

/**
 * ExportHistory Service Implementation
 */
@Service
public class ExportHistoryServiceImpl extends EntityServiceImpl<ExportHistory, Long> implements ExportHistoryService {

}