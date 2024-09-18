package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.starter.file.dto.ImportFieldDTO;

/**
 * Default handler
 */
public class DefaultHandler extends BaseImportHandler {

    public DefaultHandler(MetaField metaField, ImportFieldDTO importFieldDTO) {
        super(metaField, importFieldDTO);
    }

}
