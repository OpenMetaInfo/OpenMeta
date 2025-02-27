package info.openmeta.starter.file.service;

import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.file.dto.UploadFileDTO;
import info.openmeta.starter.file.entity.FileRecord;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;


/**
 * FileRecord service interface
 */
public interface FileRecordService extends EntityService<FileRecord, String> {

    /**
     * Upload a file to the OSS and create a FileRecord
     *
     * @param modelName the name of the corresponding business model
     * @param file the file to be uploaded
     * @return fileInfo object
     */
    FileInfo uploadFile(String modelName, MultipartFile file);

    /**
     * Upload a file to the OSS and return the fileInfo object with download URL
     * The uploadFileDTO contains the file information and input stream.
     *
     * @param uploadFileDTO the upload file DTO
     * @return filInfo object containing the download URL and metadata of the uploaded file
     */
    FileInfo uploadFile(UploadFileDTO uploadFileDTO);

    /**
     * Download the file stream from the OSS bucket by fileId
     *
     * @param fileId the ID of the file to be downloaded
     * @return the InputStream of the file
     */
    InputStream downloadStream(String fileId);

    /**
     * Get the FileInfo object by fileId
     *
     * @param fileId the ID of the file
     * @return fileInfo object with download URL
     */
    FileInfo getByFileId(String fileId);

}