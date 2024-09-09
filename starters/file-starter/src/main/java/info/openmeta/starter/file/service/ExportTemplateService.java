package info.openmeta.starter.file.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.entity.FileRecord;

/**
 * Export template service interface
 */
public interface ExportTemplateService extends EntityService<ExportTemplate, Long> {

    /**
     * Export data file according to the specified template ID
     *
     * @param templateId  template ID
     * @return exported file record
     */
    FileRecord exportByTemplate(Long templateId);
}