package info.openmeta.starter.file.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.dto.UploadFileDTO;
import info.openmeta.starter.file.entity.FileRecord;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.io.Serializable;
import java.util.List;


/**
 * FileRecord service interface
 */
public interface FileRecordService extends EntityService<FileRecord, Long> {

    /**
     * Upload the Excel bytes to the file storage.
     * @param modelName the model name
     * @param fileName the file name
     * @param excelBytes the byte array of the Excel file
     * @return the file record object
     */
    FileRecord uploadExcelBytes(String modelName, String fileName, byte[] excelBytes);

    /**
     * Upload the Excel bytes to the file storage, and return the file info object with download URL.
     * @param modelName the model name
     * @param fileName the file name
     * @param excelBytes the byte array of the Excel file
     * @return the file info object with download URL
     */
    FileInfo uploadExcelBytesToDownload(String modelName, String fileName, byte[] excelBytes);

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     * The uploadFileDTO contains the file information and input stream.
     *
     * @param uploadFileDTO the upload file DTO
     * @return the fileRecord object
     */
    FileRecord uploadFile(UploadFileDTO uploadFileDTO);

    /**
     * Upload a file to the OSS and return the fileInfo object with download URL
     * The uploadFileDTO contains the file information and input stream.
     *
     * @param uploadFileDTO the upload file DTO
     * @return a FileInfo object containing the download URL and metadata of the uploaded file
     */
    FileInfo uploadFileToDownload(UploadFileDTO uploadFileDTO);

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
    FileRecord uploadFile(String modelName, Serializable rowId, MultipartFile file);

    /**
     * Upload multiple files to the OSS and create corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param files the files to be uploaded
     * @return a list of fileRecord objects
     */
    List<FileRecord> uploadFiles(String modelName, Serializable rowId, MultipartFile[] files);

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
     * Get the FileInfo object by modelName and rowId
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @return fileInfo object with download URL
     */
    List<FileInfo> getFileInfo(String modelName, Serializable rowId);

    /**
     * Get the download URL by fileId
     *
     * @param fileId the ID of the file
     * @return the download URL
     */
    String getDownloadUrl(Long fileId);
}