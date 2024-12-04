package info.openmeta.starter.file.service;

import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.enums.FileSource;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.util.List;


/**
 * FileRecord service interface
 */
public interface FileRecordService extends EntityService<FileRecord, Long> {

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     *
     * @param modelName the name of the corresponding business model
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param fileSize the size of the file in Bytes
     * @param source the source of the file (e.g., UPLOAD, IMPORT)
     * @param inputStream the input stream of the file to be uploaded
     * @return the fileRecord object
     */
    FileRecord uploadFile(String modelName, String fileName, FileType fileType, int fileSize, FileSource source, InputStream inputStream);

    /**
     * Upload a file to the OSS and return the fileInfo object with download URL
     *
     * @param modelName the name of the corresponding business model
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param fileSize the size of the file in Bytes
     * @param inputStream the input stream of the file to be uploaded
     * @return a FileInfo object containing the download URL and metadata of the uploaded file
     */
    FileInfo uploadFileToDownload(String modelName, String fileName, FileType fileType, int fileSize, InputStream inputStream);

    /**
     * Upload a file to the OSS and create a FileRecord
     *
     * @param modelName the name of the corresponding business model
     * @param file the file to be uploaded
     * @return fileRecord object
     */
    FileRecord uploadFile(String modelName, MultipartFile file);

    /**
     * Upload a file to the OSS and create a corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param file the file to be uploaded
     * @return fileRecord object
     */
    FileRecord uploadFile(String modelName, Long rowId, MultipartFile file);

    /**
     * Upload multiple files to the OSS and create corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param files the files to be uploaded
     * @return a list of fileRecord objects
     */
    List<FileRecord> uploadFiles(String modelName, Long rowId, MultipartFile[] files);

    /**
     * Convert FileRecord to FileInfo
     *
     * @param fileRecord fileRecord object
     * @return fileInfo object
     */
    FileInfo convertToFileInfo(FileRecord fileRecord);

    /**
     * Download the file stream from the OSS bucket by fileId
     *
     * @param fileId the ID of the file to be downloaded
     * @return the InputStream of the file
     */
    InputStream downloadStream(Long fileId);

    /**
     * Get the FileInfo object by fileId
     *
     * @param fileId the ID of the file
     * @return fileInfo object with download URL
     */
    FileInfo getFileInfo(Long fileId);

    /**
     * Get the download URL by fileId
     *
     * @param fileId the ID of the file
     * @return the download URL
     */
    String getDownloadUrl(Long fileId);
}