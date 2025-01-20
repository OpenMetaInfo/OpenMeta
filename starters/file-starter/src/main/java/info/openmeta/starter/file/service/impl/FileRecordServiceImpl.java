package info.openmeta.starter.file.service.impl;

import com.github.f4b6a3.tsid.TsidCreator;
import com.google.common.collect.Lists;
import info.openmeta.framework.base.config.TenantConfig;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.SystemException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.utils.FileUtils;
import info.openmeta.starter.file.constant.FileConstant;
import info.openmeta.starter.file.dto.UploadFileDTO;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.enums.FileSource;
import info.openmeta.starter.file.oss.OSSProperties;
import info.openmeta.starter.file.oss.OssClientService;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.util.List;

/**
 * FileRecord Service Implementation
 */
@Service
@Slf4j
public class FileRecordServiceImpl extends EntityServiceImpl<FileRecord, Long> implements FileRecordService {

    @Autowired
    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    private OssClientService ossClientService;

    @Autowired
    private OSSProperties ossProperties;

    /**
     * Generate an OSS key for the file
     * ModelName is used as the prefix of the OSS key, to store files in different directories
     * Set the TSID as a part of the OSS key to avoid conflicts between files with the same name
     *
     * @param modelName the name of the corresponding business model
     * @param fileName the name of the file
     * @return the generated OSS key
     */
    public String generateOssKey(String modelName, String fileName) {
        StringBuilder key = new StringBuilder();
        // Set the subdirectory
        if (StringUtils.hasText(ossProperties.getSubDir())) {
            key.append(ossProperties.getSubDir()).append("/");
        }
        // Add tenantId as a subdirectory if multi-tenancy is enabled
        if (TenantConfig.isEnableMultiTenancy()) {
            Long tenantId = ContextHolder.getContext().getTenantId();
            if (tenantId != null) {
                key.append(tenantId).append("/");
            }
        }
        // Set the model name as a subdirectory if it is not null
        if (StringUtils.hasText(modelName)) {
            key.append(modelName).append("/");
        } else {
            key.append(FileConstant.DEFAULT_SUBFOLDER).append("/");
        }
        // Set the TSID as a part of the OSS key
        key.append(TsidCreator.getTsid()).append("/").append(fileName);
        return key.toString();
    }

    /**
     * Upload the Excel byte to the file storage.
     * @param modelName the model name
     * @param fileName the file name
     * @param excelBytes the byte array of the Excel file
     * @return the file record object
     */
    @Override
    public FileRecord uploadExcelBytes(String modelName, String fileName, byte[] excelBytes) {
        try (InputStream resultStream = new ByteArrayInputStream(excelBytes)) {
            UploadFileDTO uploadFileDTO = new UploadFileDTO();
            uploadFileDTO.setModelName(modelName);
            uploadFileDTO.setFileName(fileName);
            uploadFileDTO.setFileType(FileType.XLSX);
            uploadFileDTO.setFileSize(excelBytes.length);
            uploadFileDTO.setFileSource(FileSource.DOWNLOAD);
            uploadFileDTO.setInputStream(resultStream);
            return this.uploadFile(uploadFileDTO);
        } catch (IOException e) {
            throw new BusinessException("Error uploading Excel stream", e);
        }
    }

    /**
     * Upload the Excel byte to the file storage, and return the file info object with download URL.
     * @param modelName the model name
     * @param fileName the file name
     * @param excelBytes the byte array of the Excel file
     * @return the file info object with download URL
     */
    @Override
    public FileInfo uploadExcelBytesToDownload(String modelName, String fileName, byte[] excelBytes) {
        FileRecord fileRecord = this.uploadExcelBytes(modelName, fileName, excelBytes);
        return convertToFileInfo(fileRecord);
    }

    /**
     * Generate a full filename combining the filename, the current date and the file type extension.
     *
     * @param fileName the name of the file
     * @param fileType the type of the file
     * @return the full file name
     */
    private static String getFullFileName(String fileName, FileType fileType) {
        return fileName + "_" + DateUtils.getCurrentSimpleDateString() + fileType.getExtension();
    }

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     * The uploadFileDTO contains the file information and input stream.
     *
     * @param uploadFileDTO the upload file DTO
     * @return the fileRecord object
     */
    @Override
    public FileRecord uploadFile(UploadFileDTO uploadFileDTO) {
        String fileName = uploadFileDTO.getFileName();
        FileType fileType = uploadFileDTO.getFileType();
        String fullFileName = getFullFileName(fileName, fileType);
        String ossKey = this.generateOssKey(uploadFileDTO.getModelName(), fullFileName);
        String checksum = ossClientService.uploadStreamToOSS(ossKey, uploadFileDTO.getInputStream(), fileName);
        // Create file record
        FileRecord fileRecord = new FileRecord();
        fileRecord.setFileName(fullFileName);
        fileRecord.setFileType(uploadFileDTO.getFileType());
        fileRecord.setOssKey(ossKey);
        fileRecord.setSource(uploadFileDTO.getFileSource());
        fileRecord.setChecksum(checksum);
        fileRecord.setFileSize(uploadFileDTO.getFileSize() / 1024);
        fileRecord.setModelName(uploadFileDTO.getModelName());
        fileRecord.setRowId(uploadFileDTO.getRowId() == null ? null : uploadFileDTO.getRowId().toString());
        Long id = this.createOne(fileRecord);
        fileRecord.setId(id);
        return fileRecord;
    }

    /**
     * Upload a file to the OSS and return the fileInfo object with download URL
     * The uploadFileDTO contains the file information and input stream.
     *
     * @param uploadFileDTO the upload file DTO
     * @return a FileInfo object containing the download URL and metadata of the uploaded file
     */
    @Override
    public FileInfo uploadFileToDownload(UploadFileDTO uploadFileDTO) {
        uploadFileDTO.setFileSource(FileSource.DOWNLOAD);
        FileRecord fileRecord = this.uploadFile(uploadFileDTO);
        return convertToFileInfo(fileRecord);
    }

    /**
     * Upload a file to the OSS and create a FileRecord.
     *
     * @param modelName the name of the corresponding business model
     * @param file the file to be uploaded
     * @return fileRecord object
     */
    @Override
    public FileRecord uploadFile(String modelName, MultipartFile file) {
        return this.uploadFile(modelName, null, file);
    }

    /**
     * Upload a file to the OSS and create a corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param file the file to be uploaded
     * @return fileRecord object
     */
    @Override
    public FileRecord uploadFile(String modelName, Serializable rowId, MultipartFile file) {
        String fileName = FileUtils.getShortFileName(file);
        FileType fileType = FileUtils.getActualFileType(file);
        String fullFileName = getFullFileName(fileName, fileType);
        String ossKey = this.generateOssKey(modelName, fullFileName);
        String checksum;
        try (InputStream inputStream = file.getInputStream()) {
            checksum = ossClientService.uploadStreamToOSS(ossKey, inputStream, fileName);
        } catch (IOException e) {
            throw new SystemException("Failed to upload file {0}.", fileName + fileType.getExtension() , e);
        }
        // Create file record
        FileRecord fileRecord = new FileRecord();
        fileRecord.setModelName(modelName);
        fileRecord.setRowId(rowId == null ? null : rowId.toString());
        fileRecord.setFileName(fullFileName);
        fileRecord.setFileType(fileType);
        fileRecord.setOssKey(ossKey);
        fileRecord.setSource(FileSource.UPLOAD);
        fileRecord.setChecksum(checksum);
        fileRecord.setFileSize((int) file.getSize() / 1024);
        Long id = this.createOne(fileRecord);
        fileRecord.setId(id);
        return fileRecord;
    }

    /**
     * Upload multiple files to the OSS and create corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param files the files to be uploaded
     * @return a list of fileRecord objects
     */
    @Override
    public List<FileRecord> uploadFiles(String modelName, Serializable rowId, MultipartFile[] files) {
        List<FileRecord> fieldRecords = Lists.newArrayList();
        for(MultipartFile file : files) {
            fieldRecords.add(this.uploadFile(modelName, rowId, file));
        }
        return fieldRecords;
    }

    /**
     * Convert fileRecord object to fileInfo object
     *
     * @param fileRecord fileRecord object
     * @return fileInfo object
     */
    @Override
    public FileInfo convertToFileInfo(FileRecord fileRecord) {
        if (fileRecord == null) {
            return null;
        }
        FileInfo fileInfo = new FileInfo();
        fileInfo.setFileId(fileRecord.getId());
        fileInfo.setFileName(fileRecord.getFileName());
        fileInfo.setFileType(fileRecord.getFileType());
        String ossUrl = ossClientService.getPreSignedUrl(fileRecord.getOssKey(), fileRecord.getFileName());
        fileInfo.setUrl(ossUrl);
        return fileInfo;
    }

    /**
     * Download the file stream from the OSS bucket by fileId
     *
     * @param fileId the ID of the file to be downloaded
     * @return the InputStream of the file
     */
    @Override
    public InputStream downloadStream(Long fileId) {
        FileRecord fileRecord = this.getById(fileId)
                .orElseThrow(() -> new IllegalArgumentException("FileRecord not found by fileId {0}", fileId));
        return ossClientService.downloadStreamFromOSS(fileRecord.getOssKey(), fileRecord.getFileName());
    }


    /**
     * Get the FileInfo object by fileId
     *
     * @param fileId the ID of the file
     * @return fileInfo object with download URL
     */
    @Override
    public FileInfo getFileInfo(Long fileId) {
        FileRecord fileRecord = this.getById(fileId)
                .orElseThrow(() -> new IllegalArgumentException("FileRecord not found by fileId {0}", fileId));
        return convertToFileInfo(fileRecord);
    }

    /**
     * Get the FileInfo object by modelName and rowId
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @return fileInfo object with download URL
     */
    @Override
    public List<FileInfo> getFileInfo(String modelName, Serializable rowId) {
        Assert.notNull(rowId, "RowId cannot be null.");
        Filters filters = new Filters().eq("modelName", modelName).eq("rowId", rowId.toString());
        List<FileRecord> fileRecords = this.searchList(filters);
        return fileRecords.stream().map(this::convertToFileInfo).toList();
    }

    /**
     * Get the download URL by fileId
     *
     * @param fileId the ID of the file
     * @return the download URL
     */
    @Override
    public String getDownloadUrl(Long fileId) {
        FileRecord fileRecord = this.getById(fileId)
                .orElseThrow(() -> new IllegalArgumentException("FileRecord not found by fileId {0}", fileId));
        return ossClientService.getPreSignedUrl(fileRecord.getOssKey(), fileRecord.getFileName());
    }

}