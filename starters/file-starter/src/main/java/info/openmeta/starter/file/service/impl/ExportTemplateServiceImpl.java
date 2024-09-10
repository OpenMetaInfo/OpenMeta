package info.openmeta.starter.file.service.impl;

import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.service.ExportTemplateService;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Export template service implementation class
 */
@Service
@Slf4j
public class ExportTemplateServiceImpl extends EntityServiceImpl<ExportTemplate, Long> implements ExportTemplateService {

    @Autowired
    private FileRecordService fileRecordService;

    /**
     * Export data file according to the specified template ID
     *
     * @param templateId  template ID
     * @return exported file record
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public FileRecord exportByTemplate(Long templateId) {
        ExportTemplate template = this.readOne(templateId);
        if (template == null) {
            throw new BusinessException("Export template does not exist");
        }
        FileRecord fileRecord = fileRecordService.readOne(template.getFileId());
        if (fileRecord == null) {
            throw new BusinessException("The file record of export template {0} does not exist", template.getFileName());
        }
        return fileRecord;
    }

}