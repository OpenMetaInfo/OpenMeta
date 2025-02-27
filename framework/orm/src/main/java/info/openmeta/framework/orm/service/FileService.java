package info.openmeta.framework.orm.service;

import info.openmeta.framework.orm.domain.FileInfo;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;
import java.util.List;

public interface FileService {

    /**
     * Upload a file to the OSS and create a corresponding FileRecord to associate
     * with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId     the ID of the corresponding business row data
     * @param file      the file to be uploaded
     * @return fileInfo object
     */
    FileInfo uploadFileToRow(String modelName, Serializable rowId, MultipartFile file);

    /**
     * Upload multiple files to the OSS and create corresponding FileRecord to
     * associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId     the ID of the corresponding business row data
     * @param files     the files to be uploaded
     * @return a list of fileInfo objects
     */
    List<FileInfo> uploadFilesToRow(String modelName, Serializable rowId, MultipartFile[] files);

    /**
     * Get the fileInfos of the specified model and fileIds.
     *
     * @param modelName the model name
     * @param fileIds the file IDs
     * @return the list of fileInfo objects
     */
    List<FileInfo> getModelFiles(String modelName, List<String> fileIds);

    /**
     * Get the FileInfo object by modelName and rowId
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @return fileInfo object with download URL
     */
    List<FileInfo> getRowFiles(String modelName, Serializable rowId);

}
