package info.openmeta.starter.file.service.impl;

import com.github.f4b6a3.tsid.TsidCreator;
import com.google.common.collect.Lists;
import info.openmeta.framework.base.exception.SystemException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.utils.FileUtils;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.enums.FileSource;
import info.openmeta.starter.file.oss.OssClientService;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * FileRecord Service Implementation
 */
@Service
@Slf4j
public class FileRecordServiceImpl extends EntityServiceImpl<FileRecord, Long> implements FileRecordService {

    @Autowired
    private OssClientService ossClientService;

    /**
     * The default time-to-live (TTL) for download links in seconds.
     * Default value is 300 seconds (5 minutes).
     */
    @Value("${oss.download-ttl:300}")
    private int downloadLinkTTL;

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     *
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param fileData the byte array of the file to be uploaded
     * @param source the source of the file (e.g., UPLOAD, IMPORT)
     * @return the fileRecord object
     */
    private FileRecord uploadAndSaveRecord(String fileName, FileType fileType, byte[] fileData, FileSource source) {
        String fullFileName = fileName + "_" + DateUtils.getCurrentSimpleDateString() + fileType.getExtension();
        String ossKey = TsidCreator.getTsid() + "/" + fullFileName;
        String checksum = ossClientService.uploadByteToOSS(ossKey, fileData, fileName);
        // Create file record
        FileRecord fileRecord = new FileRecord();
        fileRecord.setFileName(fullFileName);
        fileRecord.setFileType(fileType);
        fileRecord.setOssKey(ossKey);
        fileRecord.setFileSize(fileData.length / 1024);
        fileRecord.setSource(source);
        fileRecord.setChecksum(checksum);
        Long id = this.createOne(fileRecord);
        fileRecord.setId(id);
        return fileRecord;
    }

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     * Return the fileId, without download URL for UPLOAD case.
     *
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param fileData the byte array of the file to be uploaded
     * @return fileId
     */
    @Override
    public Long uploadFile(String fileName, FileType fileType, byte[] fileData) {
        FileRecord fileRecord = uploadAndSaveRecord(fileName, fileType, fileData, FileSource.UPLOAD);
        return fileRecord.getId();
    }

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     *
     * @param fileName the name of the file to be uploaded
     * @param fileType the type of the file to be uploaded
     * @param fileData the byte array of the file to be uploaded
     * @param source the source of the file (e.g., UPLOAD, IMPORT)
     * @return fileInfo object with download URL
     */
    @Override
    public FileInfo uploadFile(String fileName, FileType fileType, byte[] fileData, FileSource source) {
        FileRecord fileRecord = uploadAndSaveRecord(fileName, fileType, fileData, source);
        return convertToFileInfo(fileRecord);
    }

    /**
     * Upload a file to the OSS and create a corresponding FileRecord.
     *
     * @param fileName the name of the file with the file extension
     * @param fileType the type of the file to be uploaded
     * @param inputStream the input stream of the file to be uploaded
     * @param source the source of the file (e.g., UPLOAD, IMPORT)
     * @return fileInfo object with download URL
     */
    @Override
    public FileInfo uploadFile(String fileName, FileType fileType, InputStream inputStream, FileSource source) {
        String fullFileName = fileName + "_" + DateUtils.getCurrentSimpleDateString() + fileType.getExtension();
        String ossKey = TsidCreator.getTsid() + "/" + fullFileName;
        String checksum = ossClientService.uploadStreamToOSS(ossKey, inputStream, fileName);
        // Create file record
        FileRecord fileRecord = new FileRecord();
        fileRecord.setFileName(fullFileName);
        fileRecord.setFileType(fileType);
        fileRecord.setOssKey(ossKey);
        fileRecord.setSource(source);
        fileRecord.setChecksum(checksum);
        Long id = this.createOne(fileRecord);
        fileRecord.setId(id);
        return convertToFileInfo(fileRecord);
    }

    /**
     * Upload a file to the OSS and create a FileRecord.
     *
     * @param fileName the name of the file to be uploaded
     * @param file the file to be uploaded
     * @return fileId
     */
    @Override
    public Long uploadFile(String fileName, MultipartFile file) {
        FileType fileType = FileUtils.getActualFileType(file);
        try {
            FileInfo fileInfo = uploadFile(fileName, fileType, file.getInputStream(), FileSource.UPLOAD);
            return fileInfo.getFileId();
        } catch (IOException e) {
            throw new SystemException("Failed to upload file {0}.", fileName + fileType.getExtension() , e);
        }
    }

    /**
     * Upload a file to the OSS and create a corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param file the file to be uploaded
     * @return fileId
     */
    @Override
    public Long uploadFile(String modelName, Long rowId, MultipartFile file) throws IOException {
        String fileName = file.getOriginalFilename();
        FileType fileType = FileUtils.getActualFileType(file);
        FileRecord fileRecord = uploadAndSaveRecord(fileName, fileType, file.getBytes(), FileSource.UPLOAD);
        return fileRecord.getId();
    }

    /**
     * Upload multiple files to the OSS and create corresponding FileRecord to associate with a business model and rowId.
     *
     * @param modelName the name of the corresponding business model
     * @param rowId the ID of the corresponding business row data
     * @param files the files to be uploaded
     * @return fileIds
     */
    @Override
    public List<Long> uploadFile(String modelName, Long rowId, MultipartFile[] files) throws IOException {
        List<Long> fileInfoList = Lists.newArrayList();
        for(MultipartFile file : files) {
            fileInfoList.add(this.uploadFile(modelName, rowId, file));
        }
        return fileInfoList;
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
        String ossUrl = ossClientService.getPreSignedUrl(fileRecord.getOssKey(), downloadLinkTTL, fileRecord.getFileName());
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
        FileRecord fileRecord = this.readOne(fileId);
        Assert.notNull(fileRecord, "FileRecord not found by fileId: {0}", fileId);
        return ossClientService.downloadStreamFromOSS(fileRecord.getOssKey(), fileRecord.getFileName());
    }

}