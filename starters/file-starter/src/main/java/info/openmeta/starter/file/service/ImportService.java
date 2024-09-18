package info.openmeta.starter.file.service;

import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.vo.ImportWizard;
import org.springframework.web.multipart.MultipartFile;

import java.util.Map;

public interface ImportService {

    /**
     * Get the fileInfo of the import template by template ID
     *
     * @param templateId template ID
     * @return import template fileInfo
     */
    FileInfo getTemplateFile(Long templateId);

    /**
     * Import data from the uploaded file and the import template ID
     *
     * @param templateId       the ID of the import template
     * @param file             the uploaded file
     * @param env the environment variables
     * @return the import result
     */
    ImportHistory importByTemplate(Long templateId, MultipartFile file, Map<String, Object> env);

    /**
     * Import data from the uploaded file and dynamic import settings
     *
     * @return the import result
     */
    ImportHistory importByDynamic(ImportWizard importWizard);
}
