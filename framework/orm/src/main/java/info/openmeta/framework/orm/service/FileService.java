package info.openmeta.framework.orm.service;

import info.openmeta.framework.orm.domain.DataFileInfo;

import java.io.Serializable;
import java.util.List;

public interface FileService {
    /**
     * Get the fileInfos of the specified field of the specified model and rowId.
     *
     * @param modelName the model name
     * @param rowIds the row IDs
     * @param fieldNames the file field names, including File and MultiFile fields
     * @return the list of DataFileInfo objects
     */
    List<DataFileInfo> getRowFiles(String modelName, List<Serializable> rowIds, List<String> fieldNames);
}
