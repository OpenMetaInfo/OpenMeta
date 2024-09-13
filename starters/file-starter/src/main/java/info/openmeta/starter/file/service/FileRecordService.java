package info.openmeta.starter.file.service;

import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.enums.FileSource;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;


/**
 * FileRecord service interface
 */
public interface FileRecordService extends EntityService<FileRecord, Long> {

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     * Return the fileId, without download URL for UPLOAD case.
     *
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param fileData the byte array of the file to be uploaded
     * @return fileId
     */
    Long uploadFile(String fileName, FileType fileType, byte[] fileData);

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     *
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param fileData the byte array of the file to be uploaded
     * @param source the source of the file (e.g., UPLOAD, IMPORT)
     * @return fileInfo object with download URL
     */
    FileInfo uploadFile(String fileName, FileType fileType, byte[] fileData, FileSource source);

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     *
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param inputStream the input stream of the file to be uploaded
     * @param source the source of the file (e.g., UPLOAD, IMPORT)
     * @return a FileInfo object containing the URL and metadata of the uploaded file
     */
    FileInfo uploadFile(String fileName, FileType fileType, InputStream inputStream, FileSource source);

    /**
     * Upload a file to the OSS and create a FileRecord
     *
     * @param fileName the name of the file to be uploaded
     * @param file the file to be uploaded
     * @return fileId
     */
    Long uploadFile(String fileName, MultipartFile file);

    /**
     * Upload a file to the OSS and create a corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param file the file to be uploaded
     * @return fileId
     */
    Long uploadFile(String modelName, Long rowId, MultipartFile file) throws IOException;

    /**
     * Upload multiple files to the OSS and create corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param files the files to be uploaded
     * @return fileIds
     */
    List<Long> uploadFile(String modelName, Long rowId, MultipartFile[] files) throws IOException;

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
}