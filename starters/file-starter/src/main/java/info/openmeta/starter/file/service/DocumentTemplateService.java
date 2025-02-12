package info.openmeta.starter.file.service;

import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.starter.file.entity.DocumentTemplate;
import info.openmeta.framework.orm.service.EntityService;

import java.io.Serializable;

/**
 * DocumentTemplate Model Service Interface
 */
public interface DocumentTemplateService extends EntityService<DocumentTemplate, Long> {

    /**
     * Generate a document according to the specified template ID and row ID.
     *
     * @param templateId template ID
     * @param rowId row ID
     * @return generated document fileInfo with download URL
     */
    FileInfo generateDocument(Long templateId, Serializable rowId);

    /**
     * Generate a document according to the specified template ID and data object.
     * The data object could be a map or a POJO.
     *
     * @param templateId  template ID
     * @param data the data object to render the document
     * @return generated document fileInfo with download URL
     */
    FileInfo generateDocument(Long templateId, Object data);

}